cd("Desktop/Erlang-Final-Batman/src").
c(mainServer).
c(computerServer).
c(batmanProtocol).
c(moveSimulator).
c(guiStateM).
mainServer:start_link(['amit@192.168.1.17','nati@192.168.1.17'],[{0,1000,0,2000},{1000,2000,0,2000}]).
