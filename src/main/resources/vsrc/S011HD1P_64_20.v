module S011HD1P_64_20(
    Q, CLK, CEB, WEB, A, D
);
parameter Bits = 20;
parameter Word_Depth = 64;
parameter Add_Width = 6;

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
