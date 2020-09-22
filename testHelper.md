cd("Desktop/Erlang-Final-Batman/src").
c(mainServer).
c(computerServer).
c(batmanProtocol).
c(moveSimulator).
c(guiStateM).
mainServer:start_link([amit@ubuntu],[{990,1010,990,1010}]).
guiStateM:start_link().
