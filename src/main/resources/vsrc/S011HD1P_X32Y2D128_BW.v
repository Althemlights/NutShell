module S011HD1P_X32Y2D128_BW(
    Q, CLK, CEB, WEB, BWEB, A, D
);
parameter Bits = 128;
parameter Word_Depth = 64;
parameter Add_Width = 6;
parameter Wen_Width = 128;

output reg [Bits-1:0] Q;
input                 CLK;
input                 CEB;
input                 WEB;
input [Wen_Width-1:0] BWEB;
input [Add_Width-1:0] A;
input [Bits-1:0]      D;

wire cen  = ~CEB;
wire wen  = ~WEB;
wire [Wen_Width-1:0] bwen = ~BWEB;

reg [Bits-1:0] ram [0:Word_Depth-1];
always @(posedge CLK) begin
    if(cen && wen) begin
        ram[A] <= (D & bwen) | (ram[A] & ~bwen);
    end
    Q <= cen && !wen ? ram[A] : {4{$random}};
end

endmodule
