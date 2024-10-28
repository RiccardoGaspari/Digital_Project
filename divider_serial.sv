module divider_serial #(
    parameter int N = 16 // Global parameter for data length
)(
    input logic clk,
    input logic resetn,
    input logic [N-1:0] IN_A,   // Dividend
    input logic [N-1:0] IN_B,   // Divisor
    output logic [N-1:0] OUT_DIV, // Quotient
    output logic [N-1:0] OUT_REM, // Remainder
    input logic start,
    output logic elab,
    output logic done
);

    logic [N-2:0] in_a_reg;
    logic [N-1:0] in_b_reg;
    logic [N-1:0] rem_d, rem_q;
    logic [N-1:0] div_d, div_q;
    logic shift, reset_cont;
    logic [N-1:0] sub;
    logic [$clog2(N):0] cont;

    typedef enum logic [1:0] {idle, elaboration, finished} stato_t;
    stato_t cs, ns;

    // Counter instantiation with dynamic width based on N
    counter #( .WIDTH($clog2(N) + 1) ) cycles_counter (
        .clk_i(clk),
        .rstn_i(resetn),
        .clear_i(reset_cont),
        .enable_i(1'b1),
        .count_o(cont)
    );

    assign reset_cont = !(resetn && shift);

    // Dividend register with asynchronous reset and enable (start)
    always_ff @(posedge clk or negedge resetn) begin
        if (!resetn)
            in_b_reg <= 0;
        else if (start)
            in_b_reg <= IN_B;
    end

    // Divisor register with asynchronous reset and shift network
    always_ff @(posedge clk or negedge resetn) begin
        if (!resetn)
            in_a_reg <= 0;
        else if (start)
            in_a_reg <= IN_A[N-2:0];
        else if (shift)
            in_a_reg <= {in_a_reg[N-3:0], 1'b0};
    end

    // Register for sampling MSB and remainder (OUT_REM corresponds to rem_q at the end)
    always_ff @(posedge clk or negedge resetn) begin
        if (!resetn)
            rem_q <= 0;
        else if (start)
            rem_q <= { {N-1{1'b0}}, IN_A[N-1] };
        else if (shift)
            rem_q <= {rem_d[N-2:0], in_a_reg[N-2]};
        else
            rem_q <= rem_d;
    end

    // Assign output remainder
    assign OUT_REM = rem_q;

    // Register for sampling quotient output (OUT_DIV)
    always_ff @(posedge clk or negedge resetn) begin
        if (!resetn)
            div_q <= 0;
        else
            div_q <= div_d;
    end

    // Assign output quotient
    assign OUT_DIV = div_q;

    // Division algorithm combinational logic
    always_comb begin
        case (cs)
            elaboration: begin
                div_d[N-1:1] = div_q[N-2:0];
                sub = rem_q - in_b_reg;

                if (rem_q >= in_b_reg) begin
                    div_d[0] = 1'b1;
                    rem_d = sub;
                end else begin
                    div_d[0] = 1'b0;
                    rem_d = rem_q;
                end
            end
            default: begin
                div_d = 16'b0;
                rem_d = 16'b0;
                sub = 16'b0;
            end
        endcase
    end

    // FSM output signals
    always_comb begin
        elab = 1'b0;
        done = 1'b0;

        case (cs)
            elaboration: elab = 1'b1;
            finished: done = 1'b1;
            default: begin
                elab = 1'b0;
                done = 1'b0;
            end
        endcase
    end

    // FSM state register
    always_ff @(posedge clk or negedge resetn) begin
        if (!resetn)
            cs <= idle;
        else
            cs <= ns;
    end

    // FSM combinational next state logic
    always_comb begin
        ns = idle;
        shift = 1'b0;

        case (cs)
            idle: begin
                if (start)
                    ns = elaboration;
            end
            elaboration: begin
                if (cont < N-1) begin
                    shift = 1'b1;
                    ns = elaboration;
                end else
                    ns = finished;
            end
            finished: begin
                if (start)
                    ns = elaboration;
            end
            default: ns = idle;
        endcase
    end

endmodule

