# **Distributed Operating Systems - Project 3**

## Chord - P2P System and Simulation

### **Group Members-**
* **Anurag Patil**
* **Pratik Kamble**

### Problem Definition
* Overlay networks can be used to provide services.
* The goal of this project is to implement the Chord protocol and a simple object access service to prove its usefulness using Erlang and Actor Model.
* The specification of the Chord protocol can be found in the paperChord: A Scalable Peer-to-peer Lookup Service for Internet Applicationsby Ion Stoica,  Robert  Morris,  David  Karger,  M.  Frans  Kaashoek,  Hari  Balakrishnan. https://pdos.csail.mit.edu/papers/ton:chord/paper-ton.pdf
* Reference to the Wikipedia page: https://en.wikipedia.org/wiki/Chord_(peer-to-peer) [Links to an external site.]
* The paper above, in section 2.3 contains a specification of the Chord API and of the API to be implemented by the application.

### Steps to run the code
* Clone this repository and install erlang.
* cd Chord--P2P_System-and-Simulation
* erl
* c(chord).
* chord:main(NumNodes, NumRequests).
* The parameters in the above command are:
  1. NumNodes: Desired number of peers to be created in the Chord peer-to-peer system.
  2. NumRequests: Desired number of requests each peer has to make.

### Conclusions and Results

1. What is working:
   1. We have implemented the Chord Protocol using Finger Tables for each node which is an optimized version than storing the next and previous node.
   2. Let's see the output with an example of **7 Nodes and 10 NumRequests**.
      1. Master Creates 7 Actor Nodes
2. What is the largest network you managed to deal with
