`default_nettype none // Required in every sv file

module miniFSM 
    (input logic rb, lb, clock, reset, 
    output logic [1:0] output);

    enum logic [1:0] {S0=2'b00, S1=2'b01, S2=2'b10, S3=2'b11} state, nextState; 

    always_ff @(posedge clock, posedge reset)
        if(reset)
            state <= S0;
        else
            state <= nextState; 
    
    always_comb //next state generator
        unique case(state)
            S0: begin
                if(rb) nextState = S1;
                else if(lb) nextState = S3;
                else nextState = S0;
            end
            S1: begin
                if(rb) nextState = S2;
                else if(lb) nextState = S0;
                else nextState = S1;
            end
            S2: begin
                if(rb) nextState = S3;
                else if(lb) nextState = S1;
                else nextState = S2;
            end
            S3: begin
                if(rb) nextState = S0;
                else if(lb) nextState = S2;
                else nextState = S3;
            end
        endcase

    //output logic
    asign output = state;

endmodule: miniFSM


module datapath 
    (input logic [3:0] userInput, 
    input logic [1:0] selectVal, 
    input logic clock, reset, resetReg, demuxEnable, 
    output logic [3:0] display0, display1, display2, display3, 
    output logic allCorrect);

    //instantiations (registers, demux, equality)
    logic [3:0] regEnable, rand0, rand1, rand2, rand3;
    logic display0_correct, display1_correct, display2_correct, display3_correct;

    //register to hold user input hex display val
    logic clear0, clear1, clear2, clear3;
    assign clear0 = resetReg && ~display0_correct;
    assign clear1 = resetReg && ~display1_correct;
    assign clear2 = resetReg && ~display2_correct;
    assign clear3 = resetReg && ~display3_correct;


    register r0(.clock(clock), .en(regEnable[0]), .clear(clear0), .Q(display0), .D(userInput));
    register r1(.clock(clock), .en(regEnable[1]), .clear(clear1), .Q(display1), .D(userInput));
    register r2(.clock(clock), .en(regEnable[2'b10]), .clear(clear2), .Q(display2), .D(userInput));
    register r3(.clock(clock), .en(regEnable[2'b11]), .clear(clear3), .Q(display3), .D(userInput));

    //set 4 random ints, each range from 0 to 9
    // assign rand0 = 
    // assign rand1 = 
    // assign rand2 = 
    // assign rand3 = 
    //register to hold random int (which stores secretPin)
    register r4(.clock(clock), .en(1'b1), .clear(reset), .Q(rand0));
    register r5(.clock(clock), .en(1'b1), .clear(reset), .Q(rand1));
    register r6(.clock(clock), .en(1'b1), .clear(reset), .Q(rand2));
    register r7(.clock(clock), .en(1'b1), .clear(reset), .Q(rand3));

    //decoder to see which hex display is being changed 
    Decoder d0(.I(selectVal), .D(regEnable), .en(demuxEnable));

    //check the equality of the guess against the secret pin only if enabled
    equality eq0(.A(display0), .B(rand0), .AeqB(display0_correct));
    equality eq1(.A(display1), .B(rand1), .AeqB(display1_correct));
    equality eq2(.A(display2), .B(rand2), .AeqB(display2_correct));
    equality eq3(.A(display3), .B(rand3), .AeqB(display3_correct));

    assign allCorrect = display1_correct && display2_correct && 
    display0_correct && display3_correct;


endmodule: datapath 

module mainFSM 
    (input logic clock, reset, switch0, switch1, switch2, switch3, 
    input logic centerButton, downButton, rb, lb, allCorrect,  
    output logic resetReg, demuxEnable, 
    output logic [24:0] leds); 

    enum logic [2:0] {RESETSTATE, SETPIN, CHANGEH0, CHANGEH1, CHANGEH2, 
                    CHANGEH3, CHECKEQ, GAMEOVER} state, nextState; 

    always_ff @(posedge clock, posedge reset)
        if(reset)
            state <= RESETSTATE;
        else
            state <= nextState; 
    
    //next state generator
    always_comb begin
        case(state)
            RESETSTATE: nextState = (reset) ? RESETSTATE : SETPIN;
            SETPIN: nextState = CHANGEH0;
            CHANGEH0: nextState = (rb) ? CHANGEH1 : state;
            CHANGEH1: begin
                if(rb) nextState = CHANGEH2; 
                else if(lb) nextState = CHANGEH0;
                else nextState = state;
            end
            CHANGEH2: begin
                if(rb) nextState = CHANGEH3; 
                else if(lb) nextState = CHANGEH1;
                else nextState = state;
            end
            CHANGEH3: begin
                if(downButton) nextState = CHECKEQ; 
                else if(lb) nextState = CHANGEH2;
                else nextState = state;
            end
            CHECKEQ: nextState = (allCorrect) ? GAMEOVER : CHANGEH0;
            GAMEOVER: nextState = (centerButton) ? RESETSTATE : state;
        endcase
    end

    //output logic
    always_comb begin
        leds = 24'b0;
        demuxEnable = 1'b0;
        case(state)
            CHANGEH0: if(centerButton) demuxEnable = 1'b1;
            CHANGEH1: if(centerButton) demuxEnable = 1'b1;
            CHANGEH2: if(centerButton) demuxEnable = 1'b1;
            CHANGEH3: if(centerButton) demuxEnable = 1'b1;
            GAMEOVER: leds = 24'b1111_1111_1111_1111_1111_1111;
        endcase
    end



endmodule: mainFSM


module register
    (input logic [7:0] D,
    input logic clock, en, clear,
    output logic [7:0] Q);

    always_ff @(posedge clock) begin
        if(en)
            Q <= D; 
        else if(clear)
            Q <= 0;
    end

endmodule: register

module Decoder
  (input  logic [1:0] I,
    input logic en, 
   output logic [3:0] D);

   always_comb begin
    D = 0;
    if (en)
      D = 1'b1 << I;
   end
  
endmodule: Decoder

module equality
  (output logic AeqB
   input  logic [3:0] A, B,
   input logic en);

    assign AeqB = (A == B);

endmodule: equality


