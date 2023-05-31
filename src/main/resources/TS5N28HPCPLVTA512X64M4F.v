module TS5N28HPCPLVTA512X64M4F(
    Q, CLK, CEB, WEB, A, D
);
parameter Bits = 64;
parameter Word_Depth = 512;
parameter Add_Width = 9;

output  reg [Bits-1:0]      Q;
input                   CLK;
input                   CEB;
input                   WEB;
input   [Add_Width-1:0] A;
input   [Bits-1:0]      D;

reg [Bits-1:0] ram [0:Word_Depth-1];
always @(posedge CLK) begin
    if(!CEB && !WEB) begin
        ram[A] <= D;
    end
    Q <= !CEB && WEB ? ram[A] : {4{$random}};
end

endmodule
