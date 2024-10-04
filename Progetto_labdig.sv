module Progetto_labdig #(
    parameter CYCLE_LIM = 100, // error threshold for the interrupt to be active
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
		// OBS.: the output is considered valid iff valid and ready are both up at the same time
		// ---> in our case valid has to be up at least 2 clock cycles
      if (bus_if.valid) begin
          // Handle Read Transaction
			 if(cs = OUT) begin
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
  end

  // Assign output signals to interface
  assign bus_if.ready = ready_reg;
  assign bus_if.error = error_reg;
  

  	// ****** INTERNAL REGISTER *******
	//
	//	interr: checks if the bitflips are occurring more than a set threshold
	//	
	//	cyclesxbf: medium time passed between any simultaneous bit flip and the next ones
	//	
	//	bfdensity: #bitflip/Tck, considering every input bit flip (can be > 1)
	//	
	
	logic interr_d, interr_q;
	logic [DATA_WIDTH-1:0] cyclesxbf_d, cyclesxbf_q;
	logic [DATA_WIDTH-1:0] bfdensity_d, bfdensity_q;
	
	always_ff @(posedge clk_i, negedge rstn_i) begin
      if(~rstn_i) begin
            interr_q <= '0;
				cyclesxbf_q <= 0;
				bfdensity_q <= 0;
      end else begin
            interr_q <= interr_d;
				cyclesxbf_q <= cyclesxbf_d;
				bfdensity_q <= bfdensity_d;
		end
   end
	
	// Interrupt output assignment
	assign interr_o = interr_q;
	
	// Register interface addresses assignment
	assign registers[0] = {{DATA_WIDTH-1{1'b0}}, interr_q};
	assign registers[1] = cyclesxbf_q;
	assign registers[2] = bfdensity_q;
	assign registers[3] = 0;
	
	// COUNTER BLOCK(s)

	localparam CNT_BITS = 64;
	
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
	
	// ****** FSM ******
	//	IDLE: waits for the first bf
	//
	//	ELAB: - stores #bitflip, #cycles, #flag_in
	//	      - if the read operation is requested, computes the outputs
	//
	//	OUT: - tells that the outputs are correctly computed and available in the Register Interface
	//		  - returns to IDLE, in which the internal registers and the interrupt are resetted
	//
	// *****************
	
    localparam int unsigned NUM_STATES = 3;
    localparam int unsigned STATES_BITS = $clog2(NUM_STATES);

    typedef enum logic [STATES_BITS-1:0] {IDLE, ELAB, OUT} state_t;
    state_t cs, ns;

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
					if(bus_if.valid)
						ns = OUT;
					else
					ns = cs;
            end
		  OUT:
				begin
					ns = IDLE;
				end
        endcase
    end
	 
	 // Computation intermediate variables
	 integer count;
	 logic [CNT_BITS-1:0] cyclesxbf_temp;
	 
	 // Outputs and counters computation
	 always_comb begin
			cyclesxbf_temp = 0;
			count = 0;
			
			case(cs)
			ELAB:
				begin
				
					cyclesxbf_d = cyclesxbf_q;
					bfdensity_d = bfdensity_q;
					interr_d = interr_q;
					bf_tot_d = bf_tot_q;
					
					clear_cycles = 0;
					clear_bf = 0;
					en_cycles = 1;
					en_bf = 0;
					
					if(flag_in) begin
						en_bf = 1;
						
						// Loop through the array and count the number of 1s
						for (int i = 0; i < DATA_WIDTH; i++) begin
						  if (input_q[i] == 1) begin
							 count = count + 1;
						  end
						end
						
						bf_tot_d = bf_tot_q + count;
						
						cyclesxbf_temp = count_cycles / count_bf;
						
						if(cyclesxbf_temp > CYCLE_LIM)
							interr_d = 1;
						
					end
					
					if(bus_if.valid) begin
						cyclesxbf_d = logic'((count_cycles / count_bf) & ((1 << DATA_WIDTH) - 1));
						bfdensity_d = logic'((bf_tot_d / logic'({{CNT_BITS{1'b0}}, count_cycles})) & ((1 << DATA_WIDTH) - 1));
					end

				end
			IDLE:
				begin
					bfdensity_d = 0;
					cyclesxbf_d = 0;
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
			default:
				begin
					bfdensity_d = bfdensity_q;
					cyclesxbf_d = cyclesxbf_q;
					interr_d = interr_q;
					bf_tot_d = bf_tot_q;
					
					clear_cycles = 1;
					clear_bf = 1;
					en_cycles = 0;
					en_bf = 0;
				end
			endcase
	 	end

endmodule