module Progetto_labdig #(
    parameter CYCLE_LIM = 100, // error threshold for the interrupt to be active
    parameter IN_DATA_WIDTH = 100, // number of input scrubs
    parameter ADDR_WIDTH = 1,  // Just 2 internal registers // To be the same as REG_BUS
	 parameter DATA_WIDTH = 32 // 32-bit data width (TO BE DEFINED, ASSUMING A MAXIMUM TIME BETWEEN BIT FLIP OF 2^32-1 cycles // To be the same as REG_BUS
	 // OSS.: the number of bits of DATA_WIDTH is related to the #bits of the counter (MAX_CNT below)
)(
    input   logic                  clk_i,
    input   logic                  rstn_i,
    input   logic [IN_DATA_WIDTH-1:0]    scrub_i,
	 REG_BUS.in bus_if
);

	// REGISTER INTERFACE CONTROL
   
	// Internal register array (for simplicity, 16 registers)
  logic [DATA_WIDTH-1:0] registers [0:(1<<ADDR_WIDTH)-1];

  // Internal signals
  logic ready_reg, error_reg;

  // Reset logic and initialization
  always_ff @(posedge clk_i or negedge rstn_i) begin
    if (!rstn_i) begin
      ready_reg <= 0;
      error_reg <= 0;
      bus_if.rdata <= {DATA_WIDTH{1'b0}};
    end else begin
      ready_reg <= 0;
      error_reg <= 0;

      // Check if a valid transaction is occurring
      if (bus_if.valid) begin
          // Handle Read Transaction
          if (bus_if.addr < (1 << ADDR_WIDTH)) begin
            // Return the data from the selected register
            bus_if.rdata <= registers[bus_if.addr];
            ready_reg <= 1;  // Indicate read success
          end else begin
            // Address out of range
            error_reg <= 1;
            bus_if.rdata <= 32'hDEADBEEF; // Arbitrary error value
          end
      end
    end
  end

  // Assign output signals to interface
  assign bus_if.ready = ready_reg;
  assign bus_if.error = error_reg;
  

  	// INTERNAL REGISTER
	logic interr_d, interr_q;
	logic [DATA_WIDTH-1:0] cyclesxbf_d, cyclesxbf_q;
	
	
	always_ff @(posedge clk_i, negedge rstn_i) begin
      if(~rstn_i) begin
            interr_q <= '0;
				cyclesxbf_q = 0;
      end else begin
            interr_q <= interr_d;
				cyclesxbf_q <= cyclesxbf_d;
		end
   end
	
	// Register interface addresses assignment
	assign registers[0] = {{DATA_WIDTH-1{1'b0}}, interr_q};
	assign registers[1] = cyclesxbf_q;
	
	// to be done comb section of internal registers

	// COUNTER BLOCK(s)
	// to add the counter of #bf
	
	localparam int MAX_CNT = 32'hFFFFFFFF; // (TO BE DEFINED) maximum count for the cycles/bf computation (see above in DATA_WIDTH)
	localparam CNT_BITS = $clog2(MAX_CNT);

	logic [CNT_BITS-1:0] count_cycles;
	logic clear_cycles, en_cycles;

	logic [CNT_BITS-1:0] count_bf;
	logic clear_bf, en_bf;
	
	counter #( .WIDTH(CNT_BITS) )
	cycles_counter (
		.clk_i   ( clk_i  ),
		.rstn_i  ( rstn_i ),
		.clear_i ( clear_cycles  ),
		.enable_i( en_cycles   ),
		.count_o ( count_cycles  )
	);
	
	counter #( .WIDTH(CNT_BITS) )
	bf_counter (
		.clk_i   ( clk_i  ),
		.rstn_i  ( rstn_i ),
		.clear_i ( clear_bf  ),
		.enable_i( en_bf   ),
		.count_o ( count_bf  )
	);
	
	// INPUT SAMPLING
	
	logic [IN_DATA_WIDTH-1:0] input_q;
	
	always_ff @(posedge clk_i or negedge rstn_i) begin
		if (!rstn_i) begin
			input_q <= {IN_DATA_WIDTH-1{1'b0}};
		end else begin
			input_q <= scrub_i;
		end
	end
	
	logic flag_in;
	assign flag_in = |input_q; // ASSUMING THAT MULTIPLE BF IN THE SAME CYCLE ARE COUNTED AS A SINGLE ONE
	
	// FSM
	
    localparam int unsigned NUM_STATES = 2;
    localparam int unsigned STATES_BITS = $clog2(NUM_STATES);

    typedef enum logic [STATES_BITS-1:0] {IDLE, ELAB} state_t;
    state_t cs, ns;

    always_ff @(posedge clk_i, negedge rstn_i) begin
        if(~rstn_i)
            cs <= IDLE;
        else
            cs <= ns;
    end

	logic [DATA_WIDTH-1:0] temp_dividend;
    logic [DATA_WIDTH-1:0] temp_quotient;
	 
	 // FSM ns computation
    always_comb begin
        case(cs)
        IDLE:
            begin
					if(flag_in)
						ns = ELAB;
					else
						ns = cs;
            end
        ELAB:
            begin
					if(bus_if.valid)
						ns = IDLE;
					else
					ns = cs;
            end
        endcase
    end
	 
	 // FSM output and counters computation
	 always_comb begin

			cyclesxbf_d = cyclesxbf_q;
			interr_d = 0;
			clear_cycles = 0;
			clear_bf = 0;
			en_cycles = 0;
			en_bf = 0;
					
			case(cs)
			ELAB:
				begin
					en_cycles = 1;

					if(flag_in) begin
						en_bf = 1;
						clear_cycles = 1;

						if (count_bf != 0) begin  // Ensure no division by zero
							cyclesxbf_d = ((cyclesxbf_q * (count_bf - 1) + count_cycles) + count_bf - 1) / count_bf;
						end else begin
							cyclesxbf_d = count_cycles;  // Handle case where count_bf is 0
						end

					end else begin
						cyclesxbf_d = cyclesxbf_q;
					end

				end
			default:
				begin
					cyclesxbf_d = cyclesxbf_q;
					interr_d = 0;
					clear_cycles = 0;
					clear_bf = 0;
					en_cycles = 0;
					en_bf = 0;
				end
			endcase
	 	end

endmodule