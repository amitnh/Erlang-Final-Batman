# Erlang-Final-Project-Batman - Protocol
this project is simulating an envierment of bi-directional links that transfer messages from one to another via BATMAN protocol.
it can work on 2-5 diffrent nodes (computers)

![alt text](https://i.ibb.co/RSLwVn9/4PC.png)

Youtube Link: https://youtu.be/pucyr1thcpM

**The Protocol:**
 B.A.T.M.A.N. is a proactive routing protocol for Wireless Ad-hoc Mesh
   Networks.  The protocol proactively maintains information about the
   existence of all nodes in the mesh that are accessible.
   The strategy of B.A.T.M.A.N. is to
   determine for each destination in the mesh one single-hop neighbor,
   which can be utilized as best gateway to communicate with the
   destination node.  
   To learn about the best next-hop for each destination is all that the B.A.T.M.A.N. algorithm cares about.
   There is no need to find out or calculate the complete route, which
   makes a very fast and efficient implementation possible.
   
   **how to start:**
   1. open 2-5 erlang nodes with short/long names
   2. compile all the files in /src
   3. run the function: 
   mainServer:start_link(computerNodes,Areas).
   computerNodes = list of nodes you opened in 1.
   Areas = list of Areas, each Area looks like this: {StartofX,EndOfX,StartOfY,EndOfY}
   each of X or Y :  have to start with 0 or end with 2000
   
   *for example:*
   1. Start 3 nodes with: erl -sname amit, erl -sname nati, erl -sname tal
   2. Compile all the files
   3. On tal@ubuntu run the command: mainServer:start_link([amit@ubuntu,nati@ubuntu],[{0,1000,0,2000},{1000,2000,0,2000}]).
   
   Or for example with 5 computer: mainServer:start_link([a@ubuntu, b@ubuntu, c@ubuntu, d@ubuntu],[{0,1000,0,1000},{1000,2000,0,1000},{0,1000,1000,2000},{1000,2000,1000,2000}]).
   
   
   
this project was written and tested on:
Ubuntu 18.04.5 LTS
Erlang OTP 20
