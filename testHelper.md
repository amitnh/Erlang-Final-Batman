cd("Desktop/Erlang-Final-Batman/src").
c(mainServer).
c(computerServer).
c(batmanProtocol).
c(moveSimulator).
c(guiStateM).
mainServer:start_link([amit@ubuntu, nati@ubuntu,moris@ubuntu, nastia@ubuntu],[{0,1000,0,1000},{1000,2000,0,1000},{0,1000,1000,2000},{1000,2000,1000,2000}]).
