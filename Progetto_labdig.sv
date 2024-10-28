module Progetto_labdig #(
    parameter INTERR_THRESHOLD = 100, // error threshold for the interrupt to be active
    parameter IN_DATA_WIDTH = 100 // number of input scrubs
)(
    input   logic                  clk_i,
    input   logic                  rstn_i,
    input   logic [IN_DATA_WIDTH-1:0]    scrub_i,
	 REG_BUS.in bus_if,
	 output logic interr_o
);

    localparam int ADDR_WIDTH = 2;  // Just 2 internal registers // TO BE THE SAME OF REG_BUS
	 localparam int DATA_WIDTH = 32; // 32-bit data width (TO BE DEFINED, ASSUMING A MAXIMUM TIME BETWEEN BIT FLIP OF 2^32-1 cycles // TO BE THE SAME OF REG_BUS  

	 // Initial Tck before starting to evaluate the cyclexbf threshold
	 localparam int INTERR_EVAL = 50;

	
    localparam int unsigned NUM_STATES = 3;
    localparam int unsigned STATES_BITS = $clog2(NUM_STATES);

    typedef enum logic [STATES_BITS-1:0] {IDLE, ELAB, OUT} state_t;
    state_t cs, ns;
	 
  	// ****** INTERNAL REGISTERS *******
	//
	//	interr: checks if the bitflips are occurring more than a set threshold
	//	
	//	cyclesxbf: medium time passed between any simultaneous bit flip and the next ones
	//	
	//	bfdensity: #bitflip/Tck, considering every input bit flip (can be > 1)
	//	
	
	logic interr_d, interr_q;
	logic [DATA_WIDTH-1:0] cyclesxbf_reg;
	logic [DATA_WIDTH-1:0] bfdensity_reg;
	
	always_ff @(posedge clk_i, negedge rstn_i) begin
      if(~rstn_i) begin
            interr_q <= '0;
      end else begin
            interr_q <= interr_d;
		end
   end
	
	// Interrupt output assignment
	assign interr_o = interr_q;

	// Internal register array
  logic [DATA_WIDTH-1:0] registers [0:(1<<ADDR_WIDTH)-1];
	
	// Register interface addresses assignment
	assign registers[0] = {{DATA_WIDTH-1{1'b0}}, interr_q};
	assign registers[1] = cyclesxbf_reg;
	assign registers[2] = bfdensity_reg;
	assign registers[3] = 0;
	
	// COUNTER BLOCK(s)

	localparam CNT_BITS = 32;
	
	// Counts the total number of cycles intercurred from the first bf in IDLE
	logic [CNT_BITS-1:0] count_cycles;
	logic clear_cycles, en_cycles;
	
	// Counts the number of cycles in which a bit flip has occurred
	logic [CNT_BITS-1:0] count_bf;
	logic clear_bf, en_bf;
	
	// Memorize how many bf there have been since the first one (bit by bit)
	logic [2*CNT_BITS-1:0] bf_tot_d, bf_tot_q;
	
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
	
	always_ff @(posedge clk_i or negedge rstn_i) begin
		if (!rstn_i) begin
			bf_tot_q <= 0;
		end else begin
			bf_tot_q <= bf_tot_d;
		end
	end
	
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
	assign flag_in = |input_q; // if flag_in = 1 there has been any number of bit flips in the input

	// SERIAL DIVIDERS

	// Control logic to compute the division start
	// (due to the divider contraint, start has to be up only for 1 Tck)
	logic start_flag, control_flag;

	always_ff @(posedge clk_i or negedge rstn_i) begin
		if (!rstn_i) begin
			start_flag <= 0;
			control_flag <= 0;
		end else begin
			if(!control_flag & bus_if.valid) begin
				start_flag <= 1;
				control_flag <= 1;
			end
			if(start_flag)
				start_flag <= 0;
			if(cs == IDLE)
				control_flag <= 0;
		end
	end

    logic done_cxbf, done_bfd;
	 logic [CNT_BITS-1:0] cxbf;
	 logic [2*CNT_BITS-1:0] bfd;

    divider_serial #( .N(CNT_BITS) )
	 cyclesxbf_divider_serial (
        .clk(clk_i),
        .resetn(rstn_i),
        .IN_A(count_cycles),
        .IN_B(count_bf),
        .OUT_DIV(cxbf),
        .start(start_flag),
		  .done(done_cxbf)
    );
	 
    divider_serial #( .N(2*CNT_BITS) )
	 bfdensity_divider_serial (
        .clk(clk_i),
        .resetn(rstn_i),
        .IN_A(bf_tot_q),
        .IN_B({{CNT_BITS{1'b0}}, count_cycles }),
        .OUT_DIV(bfd),
        .start(start_flag),
		  .done(done_bfd)
    );
	 // obs.: done is controlled by the bigger divider (division time: IN_LENGHT*Tck)
	logic [DATA_WIDTH-1:0] cyclesxbf_d, cyclesxbf_q;
	logic [DATA_WIDTH-1:0] bfdensity_d, bfdensity_q;

	assign cyclesxbf_reg = cyclesxbf_q;
	assign bfdensity_reg = bfdensity_q;

	always_ff @(posedge clk_i or negedge rstn_i) begin
		if (!rstn_i) begin
			cyclesxbf_q <= 0;
		end else begin
			cyclesxbf_q <= cyclesxbf_d;
		end
	end
	
	always_ff @(posedge clk_i or negedge rstn_i) begin
		if (!rstn_i) begin
			bfdensity_q <= 0;
		end else begin
			bfdensity_q <= bfdensity_d;
		end
	end

	always_comb begin
		cyclesxbf_d = cyclesxbf_q;
		if(done_cxbf)
			cyclesxbf_d = cxbf[DATA_WIDTH-1:0];
	end

	always_comb begin
		bfdensity_d = bfdensity_q;
		if(done_bfd)
			bfdensity_d = bfd[DATA_WIDTH-1:0];
	end
	
	// ****** FSM ******
	//
	//	IDLE: waits for the first bf
	//
	//	ELAB: - stores #bitflip, #cycles, #flag_in
	//	      - if the read operation is requested, computes the outputs
	//
	// *****************

    always_ff @(posedge clk_i, negedge rstn_i) begin
        if(~rstn_i)
            cs <= IDLE;
        else
            cs <= ns;
    end
	 
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
					if(done_bfd)
						ns = OUT;
					else
					ns = cs;
            end
				
		  OUT: ns = IDLE;
		  
		  default: ns = IDLE;
		  
        endcase
    end
	 
	 // Computation intermediate variables
	 integer count, msb_index;
	 logic [CNT_BITS-1:0] cyclesxbf_temp;
	 logic greaterpw2;
	 
	 // Outputs and counters computation
	 always_comb begin
			cyclesxbf_temp = 0;
			count = 0;
			msb_index = 0;
			greaterpw2 = 0;
			
			case(cs)
			ELAB:
				begin
				
					interr_d = interr_q;
					bf_tot_d = bf_tot_q;
					
					clear_cycles = 0;
					clear_bf = 0;
					en_cycles = 1;
					en_bf = 0;
					
					if(flag_in) begin
						en_bf = 1;
						
						// Loop through the array and count the number of 1s
						for (int i = 0; i < IN_DATA_WIDTH; i++) begin
						  if (input_q[i] == 1) begin
							 count = count + 1;
						  end
						end
						
						bf_tot_d = bf_tot_q + count;
						
					  for (int i = CNT_BITS-1; i >= 0; i--) begin
							if (count_bf[i] & msb_index == 0) begin
								 msb_index = i;
							end
							if (count_bf[i] & msb_index != 0) begin
								 greaterpw2 = 1;
								 break;
							end
					  end
					  
						// Divides by count_bf rounded by the nearest smaller power of 2
						cyclesxbf_temp = count_cycles >> (msb_index + greaterpw2);
						
						// The interrupt evaluation starts after a set #cycles
						if(count_cycles > INTERR_EVAL & cyclesxbf_temp < INTERR_THRESHOLD)
							interr_d = 1;
						
					end

				end
			IDLE:
				begin
					interr_d = 0;
					bf_tot_d = 0;
					
					clear_cycles = 1;
					clear_bf = 1;
					en_cycles = 0;
					en_bf = 0;
					
					if(flag_in) begin

						// Loop through the array and count the number of 1s
						for (int i = 0; i < DATA_WIDTH; i++) begin
						  if (input_q[i] == 1) begin
							 count = count + 1;
						  end
						end
						// bf_tot initialization
						bf_tot_d = count;
						
						// Start counting #cycles and #flag_in
						clear_cycles = 0;
						clear_bf = 0;
						en_cycles = 1;
						en_bf = 1;
						
					end
				end
			OUT:
				begin
					interr_d = 0;
					bf_tot_d = 0;
					
					clear_cycles = 1;
					clear_bf = 1;
					en_cycles = 0;
					en_bf = 0;
				end
			default:
				begin
					interr_d = 0;
					bf_tot_d = 0;
					
					clear_cycles = 1;
					clear_bf = 1;
					en_cycles = 0;
					en_bf = 0;
				end
			endcase
	 	end
		
	// REGISTER INTERFACE CONTROL

  // Internal signals
  logic ready_reg;

  // Reset logic and initialization
  always_comb begin
    if (!rstn_i) begin
      ready_reg <= 0;
      bus_if.rdata <= {DATA_WIDTH{1'b0}};
    end else begin
      ready_reg <= 0;
      bus_if.rdata <= {DATA_WIDTH{1'b0}};

      // Check if a valid transaction is occurring
		// OBS.: the output is considered valid iff valid and ready are both up at the same time
		// ---> in our case valid has to be up at least 2 clock cycles
      if (cs == OUT) begin
			// Handle Read Transaction
			// Return the data from the selected register
			bus_if.rdata <= registers[bus_if.addr];
			ready_reg <= 1;
      end
    end
  end

  // Assign output signals to interface
  assign bus_if.ready = ready_reg;
  
  // Eventually set the error flag to 1 if the state isn't in the respected range
  always_comb begin
		if(cs != OUT || cs != ELAB || cs != IDLE)
			bus_if.error = 1;
		else
			bus_if.error = 0;
  end

endmodule