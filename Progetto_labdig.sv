module Progetto_labdig #(
    parameter CYCLE_LIM = 100, 	//Frequency threshold for the interrupt to be active
    parameter IN_DATA_WIDTH = 100 	//Number of input data
	parameter ADDR_WIDTH = 2,
	parameter DATA_WIDTH = 32
)(
    input   logic           clk_i,
    input   logic           rstn_i,
    input   logic [IN_DATA-1:0] value_i,
    output  logic    		 interr_o,
	output  logic [15:0]    value_o
);	

	//Register Interface

	//Internal register array (for simplicity, 16 registers)
  	logic [DATA_WIDTH-1:0] registers [0:(1<<ADDR_WIDTH)-1];
	// Internal signals
  	logic ready_reg, error_reg;
	//Reset logic and initialization
  	always_ff @(posedge clk_i or negedge rstn_i) 
	begin
    	if (!rstn_i) begin
      		ready_reg <= 0;
      		error_reg <= 0;
      		bus_if.rdata <= {DATA_WIDTH{1'b0}};
    	end else begin
     		ready_reg <= 0;
      		error_reg <= 0;

      		//Check if a valid transaction is occurring
      		if (bus_if.valid) 
			begin
        	//Handle Read Transaction
          	if (bus_if.addr < (1 << ADDR_WIDTH)) 
			begin
            	//Return the data from the selected register
            	bus_if.rdata <= registers[bus_if.addr];
            	ready_reg <= 1;  //Indicate read success
        	end else begin
            	//Address out of range
            	error_reg <= 1;
            	bus_if.rdata <= 32'hDEADBEEF; //Arbitrary error value
          	end
      		end
    	end
 	 end

  	//Assign output signals to interface
  	assign bus_if.ready = ready_reg;
  	assign bus_if.error = error_reg;



	/* # COUNTER # */

	//Counter total cicles 
	localparam int MAX_CNT = 1e5; 
	localparam CNT_BITS = $clog2(MAX_CNT);
	logic [CNT_BITS-1:0] count_tot;
	logic clear_tot;
	
	counter #( .WIDTH(CNT_BITS) )
	i_counter (
		.clk_i   ( clk_i ),
		.rstn_i  ( rstn_i ),
		.clear_i ( clear ),
		.enable_i ( 1'b1 ),
		.count_o ( count_tot )
	);
	

	//Counter for single glitch (+1 for avery cicle where there is at least one input at 1)
	logic [32:0] cnt_bf_d, cnt_bf_q;

	always_ff @(posedge clk_i or negedge rstn_i) 
	begin
		if (!rstn_i) begin
			cnt_bf_q <= 0;
		end else begin
			cnt_bf_q <= cnt_bf_d;
		end
	end


	//Counter for the actual number of glitch 
	logic [32:0] cnt_tot_bf_d, cnt_tot_bf_q;

	always_ff @(posedge clk_i or negedge rstn_i) 
	begin
		if (!rstn_i) begin
			cnt_tot_bf_q <= 0;
		end else begin
			cnt_tot_bf_q <= cnt_tot_bf_d;
		end
	end


	/* # INPUT SECTION # */

	//Flag that at least one input went to "1" defined as the OR of the input
	logic flag_in;
	assign flag_in = |input_q;


	//Input Reg
	logic[IN_DATA-1:0] input_q;	

	always_ff @(posedge clk_i or negedge rstn_i) 
	begin
		if (!rstn_i) begin
			input_q <= '0;
		end else begin
			input_q <= value_i;						//we will use input_q to eval how many bit went to "1" 
		end
	end


	/* # OUTPUT SECTION # */

	//Cycles x bitflip Reg
	logic [DATA_WIDTH-1:0] cyclesxbf_d, cyclesxbf_q;
	always_ff @(posedge clk_i or negedge rstn_i) 
	begin
		if (!rstn_i) begin
			cyclesxbf_q <= '0;
		end else begin
			cyclesxbf_q <= cyclesxbf_d;
		end
	end


	//Bitflip Density Reg
	logic [15:0] bf_density_d, bf_density_q;

	always_ff @(posedge clk_i or negedge rstn_i) 
	begin
		if (!rstn_i) begin
			bf_density_q <= '0;
		end else begin
			bf_density_q <= bf_density_d;
		end
	end


	//Output Interrupt Reg
	logic interr_d, interr_q;
	always_ff @(posedge clk_i or negedge rstn_i) 
	begin
		if (!rstn_i) begin
			interr_q <= '0;
		end else begin
			interr_q <= interr_d;
		end
	end
	//Out Interrupt 
	assign interr_o = interr_q;


	//FSM
	localparam int unsigned NUM_STATES = 2;
    localparam int unsigned STATES_BITS = $clog2(NUM_STATES);
	typedef enum logic [STATES_BITS-1:0] {IDLE, ELAB} state_t;
    state_t cs, ns;
	
	always_ff @(posedge clk_i or negedge rstn_i) 
	begin
		if (!rstn_i) begin
			cs <= IDLE;
		end else begin
			cs <= ns;
		end
	end

	//FSM COMB
	always_comb 
	begin
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
		OUT: 
			begin
				ns = IDLE;
			end 
        endcase
    end


	always_comb
	begin
		//Counter
		clear_tot = '0;
		cnt_bf_d = cnt_bf_q;
		cnt_tot_bf_d = cnt_tot_bf_q;
		//Internal Val
		cyclesxbf_d = cyclesxbf_q;
		bf_density_d = bf_density_q;
		interr_d = interr_q;
		//FSM
		ns = cs;


		case(cs)

		IDLE:
			begin
				if(flag_in)
				begin
					//Starting the total cicles counter
					clear_tot = '0;
					//Starting the counter of times we had at least one bf
					cnt_bf_d++; 
					//Starting the counter of actual n of bits
					for (logic i = 0; i < IN_DATA_WIDTH; i++) 
					begin
    					cnt_tot_bf_d += (input_q[i] != 0) ? 1 : 0;
					end
				end 
			end 

		ELAB:
		 	begin
				clear_tot = '0;

				if (flag_in)
				begin
					cnt_bf_d++; 								//Counts the n times we had any bf 

					for (logic i = 0; i < IN_DATA_WIDTH; i++) 
					begin
    					cnt_tot_bf_d += (input_q[i] != 0) ? 1 : 0;
					end
					
					//Cycle x BF
					cyclesxbf_d = (cnt_bf_q++) / count_q;  //(bitflip totali + singolo bitflip eventuale a "1")/n tot cicli
					//Density for BF cicles 
					bf_density_d = cnt_tot_bf_d / cnt_bf_q;

				end else begin

					//Mantaining the old val of cicle x bit
					cyclesxbf_d = cyclesxbf_q;  
					//Mantaining the old val of bf density
					bf_density_d = bf_density_q;
				end 

				//Interrupt Handling
				if(cyclesxbf_q > CYCLE_LIM)
					interr_d = 1'b1;					
			end
		
		OUT:
			begin
				//Resetting every counter
				clear_tot = 1'b1;
				cnt_bf_d = '0;
				cnt_tot_bf_d = '0;
			end
		endcase
	end
endmodule