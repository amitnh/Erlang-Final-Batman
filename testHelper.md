cd("Desktop/Erlang-Final-Batman/src").
c(mainServer).
c(computerServer).
c(batmanProtocol).
c(moveSimulator).
c(guiStateM).
mainServer:start_link([amit@ubuntu],[{950,1050,950,1050}]).
guiStateM:start_link().
