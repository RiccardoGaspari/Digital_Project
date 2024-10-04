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

    localparam int ADDR_WIDTH = 2;  // Just 2 internal registers // To be the same as REG_BUS
	 localparam int DATA_WIDTH = 32; // 32-bit data width (TO BE DEFINED, ASSUMING A MAXIMUM TIME BETWEEN BIT FLIP OF 2^32-1 cycles // To be the same as REG_BUS
	 // OSS.: the number of bits of DATA_WIDTH is related to the #bits of the counter (MAX_CNT below)
	 
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
	logic [DATA_WIDTH-1:0] bfdensity_d, bfdensity_q; // ADD IN THE REGISTER INTERFACE
	
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
	// to add the counter of #bf
	
	localparam int MAX_CNT = 32'hFFFFFFFF; // Maximum count for the cycles/bf computation (see above in DATA_WIDTH)
	localparam CNT_BITS = $clog2(MAX_CNT);

	logic [CNT_BITS-1:0] count_cycles;
	logic clear_cycles, en_cycles;

	logic [CNT_BITS-1:0] count_bf;
	logic clear_bf, en_bf;
	
	// Counts the total number of cycles starting from the first bit flip
	logic [2*CNT_BITS-1:0] count_tot;
	logic clear_tot, en_tot;
	// Saves the total cycles of the previous bit flip instance
	logic [2*CNT_BITS-1:0] prev_ctot_d, prev_ctot_q;
	
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
	
	counter #( .WIDTH(2*CNT_BITS) )
	tot_counter (
		.clk_i   ( clk_i  ),
		.rstn_i  ( rstn_i ),
		.clear_i ( clear_tot  ),
		.enable_i( en_tot   ),
		.count_o ( count_tot  )
	);
	
	always_ff @(posedge clk_i or negedge rstn_i) begin
		if (!rstn_i) begin
			prev_ctot_q <= 0;
		end else begin
			prev_ctot_q <= prev_ctot_d;
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
	 
	 integer count;
	 
	 // FSM output and counters computation
	 always_comb begin		
			case(cs)
			ELAB:
				begin
					en_cycles = 1;
					
					cyclesxbf_d = cyclesxbf_q;
					interr_d = interr_q;
					clear_cycles = 0;
					clear_bf = 0;
					clear_tot = 0;
					en_cycles = 0;
					en_bf = 0;
					en_tot = 1;
					count = 0;
					prev_ctot_d = prev_ctot_q;
					bfdensity_d = bfdensity_q;


					if(flag_in) begin
						en_bf = 1;
						clear_cycles = 1;

						if (count_bf != 0)  // Ensure no division by zero
							cyclesxbf_d = ((cyclesxbf_q * (count_bf - 1) + count_cycles) + count_bf - 1) / count_bf;
						else
							cyclesxbf_d = count_cycles;  // Handle case where count_bf is 0
							
						// Loop through the array and count the number of 1s
						for (int i = 0; i < DATA_WIDTH; i++) begin
						  if (input_q[i] == 1) begin
							 count = count + 1;
						  end
						end
						
						bfdensity_d = (bfdensity_q * prev_ctot_q + count) / count_tot;
						
						// Updates prev_ctot register for the next iteration
						prev_ctot_d = count_tot;

					end
					
					if(cyclesxbf_d > CYCLE_LIM)
						interr_d = 1;
					else
						interr_d = interr_q;
				end
			IDLE:
				begin
					bfdensity_d = 0;
					cyclesxbf_d = 0;
					interr_d = 0;
					clear_cycles = 1;
					clear_bf = 1;
					clear_tot = 1;
					en_cycles = 0;
					en_bf = 0;
					en_tot = 0;
					count = 0;
					prev_ctot_d = 0;
					
					if(flag_in) begin
						// Loop through the array and count the number of 1s
						for (int i = 0; i < DATA_WIDTH; i++) begin
						  if (input_q[i] == 1) begin
							 count = count + 1;
						  end
						end
						bfdensity_d = count;
					end
				end
			endcase
	 	end

endmodule