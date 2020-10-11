cd("Desktop/Erlang-Final-Batman/src").
c(mainServer).
c(computerServer).
c(batmanProtocol).
c(moveSimulator).
c(guiStateM).
mainServer:start_link([alfred@192.168.1.17, bruce@192.168.1.17, fox@192.168.1.17, joker@192.168.1.18],[{0,1000,0,1000},{1000,2000,0,1000},{0,1000,1000,2000},{1000,2000,1000,2000}]).
