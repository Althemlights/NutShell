`timescale 1ns/1ps
module CKLNQD12BWP40P140LVT (TE, E, CP, Q);
    input TE, E, CP;
    output Q;
    reg notifier;
    `ifdef NTC
        wire TE_d, E_d, CP_d;
        pullup (CDN);
        pullup (SDN);
        or (D_i, E_d, TE_d);
        not (CPB, CP_d);
        tsmc_dla (Q_buf, D_i, CPB, CDN, SDN, notifier);
        and (Q, Q_buf, CP_d);
    `else
        pullup (CDN);
        pullup (SDN);
        or (D_i, E, TE);
        not (CPB, CP);
        tsmc_dla (Q_buf, D_i, CPB, CDN, SDN, notifier);
        and (Q, Q_buf, CP);
    `endif

  `ifdef TETRAMAX
  `else
    tsmc_xbuf (nTE_SDFCHK, nTE, 1'b1);
    tsmc_xbuf (nE_SDFCHK, nE, 1'b1);
    tsmc_xbuf (E_TE_SDFCHK, E_TE, 1'b1);
    tsmc_xbuf (E_nTE_SDFCHK, E_nTE, 1'b1);
    tsmc_xbuf (nE_TE_SDFCHK, nE_TE, 1'b1);
    tsmc_xbuf (nE_nTE_SDFCHK, nE_nTE, 1'b1);
    not (nTE, TE);
    not (nE, E);
    and (E_TE, E, TE);
    and (E_nTE, E, nTE);
    and (nE_TE, nE, TE);
    and (nE_nTE, nE, nTE);


  // Timing logics defined for default constraint check
    `ifdef NTC
      not  (E_int_not, E_d);
      not  (TE_int_not, TE_d);
    `else
      not  (E_int_not, E);
      not  (TE_int_not, TE);
    `endif
    buf  (E_check, TE_int_not);
    buf  (TE_check, E_int_not);
    tsmc_xbuf (E_DEFCHK, E_check, 1'b1);
    tsmc_xbuf (TE_DEFCHK, TE_check, 1'b1);

  specify
    if (E == 1'b1 && TE == 1'b1)
    (CP => Q) = (0, 0);
    if (E == 1'b1 && TE == 1'b0)
    (CP => Q) = (0, 0);
    if (E == 1'b0 && TE == 1'b1)
    (CP => Q) = (0, 0);
    if (E == 1'b0 && TE == 1'b0)
    (negedge CP => (Q+:1'b0)) = (0, 0);
    $width (posedge CP &&& E_TE_SDFCHK, 0, 0, notifier);
    $width (negedge CP &&& E_TE_SDFCHK, 0, 0, notifier);
    $width (posedge CP &&& E_nTE_SDFCHK, 0, 0, notifier);
    $width (negedge CP &&& E_nTE_SDFCHK, 0, 0, notifier);
    $width (posedge CP &&& nE_TE_SDFCHK, 0, 0, notifier);
    $width (negedge CP &&& nE_TE_SDFCHK, 0, 0, notifier);
    $width (negedge CP &&& nE_nTE_SDFCHK, 0, 0, notifier);
  `ifdef NTC
    $setuphold (posedge CP &&& nTE_SDFCHK, posedge E , 0, 0, notifier,,, CP_d, E_d);
    $setuphold (posedge CP &&& nTE_SDFCHK, negedge E , 0, 0, notifier,,, CP_d, E_d);
    $setuphold (posedge CP &&& nE_SDFCHK, posedge TE , 0, 0, notifier,,, CP_d, TE_d);
    $setuphold (posedge CP &&& nE_SDFCHK, negedge TE , 0, 0, notifier,,, CP_d, TE_d);
  `else
    $setuphold (posedge CP &&& nTE_SDFCHK, posedge E , 0, 0, notifier);
    $setuphold (posedge CP &&& nTE_SDFCHK, negedge E , 0, 0, notifier);
    $setuphold (posedge CP &&& nE_SDFCHK, posedge TE , 0, 0, notifier);
    $setuphold (posedge CP &&& nE_SDFCHK, negedge TE , 0, 0, notifier);
  `endif
  endspecify
  `endif
endmodule
