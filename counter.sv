module counter 
	#(
		parameter WIDTH = 16
	)
	(
	input logic clk_i,
	input logic rstn_i,
	input logic clear_i,
	input logic enable_i,
	output logic [WIDTH-1:0] count_o
	);
	
	logic [WIDTH-1:0] count_d, count_q;
	
	always_ff @(posedge clk_i or negedge rstn_i) begin
		if (!rstn_i) begin
			count_q <= '0;
		end else begin
			count_q <= count_d;
		end
	end
	
	always_comb begin
		count_d = count_q;
		
		if (clear_i) begin
			count_d = '0;
		end else if (enable_i) begin
			count_d = count_q + 1;
		end
	end
	
	assign count_o = count_q;
	
endmodule
