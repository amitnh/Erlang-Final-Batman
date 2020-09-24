cd("Desktop/Erlang-Final-Batman/src").
c(mainServer).
c(computerServer).
c(batmanProtocol).
c(moveSimulator).
c(guiStateM).
mainServer:start_link([amit@ubuntu],[{0,1000,0,1000}]).
guiStateM:start_link().
