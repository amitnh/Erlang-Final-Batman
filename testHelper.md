cd("Desktop/Erlang-Final-Batman/src").
c(mainServer).
c(computerServer).
c(batmanProtocol).
c(moveSimulator).
c(guiStateM).
mainServer:start_link([amit@ubuntu],[{-50,2050,-50,2050}]).
guiStateM:start_link().
