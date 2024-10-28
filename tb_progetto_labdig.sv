`timescale 1ns/1ps

module tb_progetto_labdig();

// Testbench Parameters
localparam CYCLE_LIM = 100;        // Error threshold for the interrupt
localparam IN_DATA_WIDTH = 100;    // Number of input scrubs
localparam ADDR_WIDTH = 2;
localparam DATA_WIDTH = 32;

logic clk_i, rstn_i, interr_o;
logic [IN_DATA_WIDTH-1:0] scrub_i;

logic [DATA_WIDTH-1:0] read_data;
int i, j;

// Instantiate the REG_BUS interface
REG_BUS #(
  .ADDR_WIDTH(ADDR_WIDTH),
  .DATA_WIDTH(DATA_WIDTH)
) bus_if (
  .clk_i(clk_i) // Connect the clock to the interface
);

// Instantiate the DUT
Progetto_labdig #(
  .INTERR_THRESHOLD(CYCLE_LIM),
  .IN_DATA_WIDTH(IN_DATA_WIDTH)
) DUT (
  .clk_i(clk_i),
  .rstn_i(rstn_i),
  .scrub_i(scrub_i),
  .bus_if(bus_if.in),  // Connect the `in` modport of the interface
  .interr_o(interr_o)
);

// Task to perform a read operation on the bus interface
task automatic read_register(input logic [ADDR_WIDTH-1:0] addr, output logic [DATA_WIDTH-1:0] data);
  begin
    // Set the address and valid signal
    bus_if.addr = addr;
    bus_if.valid = 1;

    // Wait for ready signal from DUT
    
    while (!bus_if.ready) begin
      @(posedge clk_i); // Wait until ready signal is asserted
    end

    // Read the data once ready
    data = bus_if.rdata;
    bus_if.valid = 0; // De-assert valid after reading

  end
endtask

// Clock Generation
always #5 clk_i = ~clk_i;

// Initial Block: Testbench Stimuli
initial begin
  // Initialize signals
  clk_i = 0;
  rstn_i = 0;
  scrub_i = 0;
  bus_if.addr = 0;
  bus_if.valid = 0;
  // Signals set to 0 by default
  bus_if.wdata = 0;
  bus_if.wstrb = 0;
  bus_if.write = 0;

  // Reset the DUT
  @(negedge clk_i);
  rstn_i = 1;

  // ADDRESS MAPPING
  // registers[0] = {{DATA_WIDTH-1{1'b0}}, interr_q};
  // registers[1] = cyclesxbf_q;
  // registers[2] = bfdensity_q;
  // registers[3] = 0;

  // FIRST ANALYSIS: random inputs
  for (i = 0; i < 15; i++) begin
    @(negedge clk_i);
    for (j = 0; j < IN_DATA_WIDTH; j++) begin
	scrub_i[j] = $urandom % 2; // Randomize each bit of scrub_i
    end
  end

  // Read data from the DUT using the read task
  read_register(2, read_data);
  $display("1st Register value = %h", read_data);
  @(negedge clk_i);

  // SECOND ANALYSIS: checking the interrupt threshold (set to 100Tck)
  scrub_i = { {(IN_DATA_WIDTH-1){1'b0}}, 1'b1 };
  @(negedge clk_i);
  scrub_i = 0;
  repeat (98) @(negedge clk_i);
  scrub_i = { {(IN_DATA_WIDTH-1){1'b0}}, 1'b1 };
  @(negedge clk_i);
  scrub_i = 0;
  // If correctly set, the interrupt should be set to 1
  // Read data from the DUT using the read task
  read_register(0, read_data);
  $display("2nd Register value = %h", read_data);
  @(negedge clk_i);

  // COUNTEREXAMPLE
  scrub_i = { {(IN_DATA_WIDTH-1){1'b0}}, 1'b1 };
  @(negedge clk_i);
  scrub_i = 0;
  repeat (99) @(negedge clk_i);
  scrub_i = { {(IN_DATA_WIDTH-1){1'b0}}, 1'b1 };
  @(negedge clk_i);
  scrub_i = 0;
  // If correctly set, the interrupt should stay at 0
  // Read data from the DUT using the read task
  read_register(0, read_data);
  $display("3rd Register value = %h", read_data);
  @(negedge clk_i);

  // TESTING CYCLESXBF
  scrub_i = { {(IN_DATA_WIDTH-1){1'b0}}, 1'b1 };
  @(negedge clk_i);
  scrub_i = 0;
  repeat (96) @(negedge clk_i);
  scrub_i = { {(IN_DATA_WIDTH-1){1'b0}}, 1'b1 };
  @(negedge clk_i);
  scrub_i = 0;
  // If correctly set, the interrupt should stay at 0
  // Read data from the DUT using the read task
  read_register(1, read_data);
  $display("4th Register value = %h", read_data);
  @(negedge clk_i);

  $stop;
end

// Monitor signals
initial begin
  $monitor("Time = %0t, scrub_i = %b, interr_o = %b",
            $time, scrub_i, interr_o);
end

endmodule

