module Progetto_labdig #(
    parameter FREQ_LIM = 100, // frequency threshold for the interrupt to be active
    parameter IN_DATA = 100 // number of input data
)(
    input   logic           clk_i,
    input   logic           rstn_i,
    input   logic [IN_DATA-1:0]    value_i,
    output  logic    		 interr_o,
	 output  logic [15:0]    freq_o
);

	// COUNTER BLOCK
	
	localparam int MAX_CNT = 1e5; // (TO BE DEFINED) maximum count for the frequency computation
	localparam CNT_BITS = $clog2(MAX_CNT);

	logic [CNT_BITS-1:0] count;
	logic clear;
	
	counter #( .WIDTH(CNT_BITS) )
	i_counter (
		.clk_i   ( clk_i  ),
		.rstn_i  ( rstn_i ),
		.clear_i ( clear  ),
		.enable_i( 1'b1   ),
		.count_o ( count  )
	);
	
	// INPUT SAMPLING
	
	logic [IN_DATA-1:0] input_q;
	
	always_ff @(posedge clk_i or negedge rstn_i) begin
		if (!rstn_i) begin
			input_q <= '0;
		end else begin
			input_q <= value_i;
		end
	end


endmodule