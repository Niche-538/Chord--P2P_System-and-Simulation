# **Distributed Operating Systems - Project 3**

## Chord - P2P System and Simulation

### **Group Members-**
* **Anurag Patil**
* **Pratik Kamble**

### Problem Definition
* Overlay networks can be used to provide services.
* This project aims to implement the Chord protocol and a simple object access service to prove its usefulness using Erlang and its Actor Model.
* The specification of the Chord protocol can be found in the paperChord: A Scalable Peer-to-peer Lookup Service for Internet Applicationsby Ion Stoica, Robert Morris, David Karger, M. Frans Kaashoek, Hari Balakrishnan. https://pdos.csail.mit.edu/papers/ton:chord/paper-ton.pdf
* Reference to the Wikipedia page: https://en.wikipedia.org/wiki/Chord_(peer-to-peer)
* The paper above, in section 2.3, contains a specification of the Chord API and the API to be implemented by the application.

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
   1. We have implemented the Chord Protocol using Finger Tables for each node, an optimized version storing the next and previous node.
   2. Let's see the output with an example of **7 Nodes and 10 NumRequests**:
      1. Master Creates 7 Actor Nodes<br>
      <img width="432" alt="Screenshot 2022-10-23 at 22 41 02" src="https://user-images.githubusercontent.com/54627841/197438191-07f034cc-ec0b-4428-a4c6-92e8a1faf672.png"><br>
      2. As we have 7 Nodes, The range of values we have is from 1-127, Hence every actor is assigned an Identifier in this range.<br>
      <img width="235" alt="Screenshot 2022-10-23 at 22 42 23" src="https://user-images.githubusercontent.com/54627841/197438261-70e5d635-a229-4855-9ec8-6876bfa4d3cb.png"><br>
      3. Every Actor creates a Finger Table according to the protocol. Finger table stores the PID of the node whose Identifier <= CurrentID + 2^M<br>
      <img width="653" alt="Screenshot 2022-10-23 at 22 40 23" src="https://user-images.githubusercontent.com/54627841/197438074-0861c04f-3e1a-4e8f-a45a-324753cc6444.png"><br>
      4. After Finger Table, all actors are created, and the Master signals the actors to start the protocol to send NumRequest (10 in this example) requests.
      5. A random string is generated for each actor for insertion, the string is hashed using SHA-1, and the hash is further divided by 2^7 to bring it in the range.<br>
      <img width="585" alt="Screenshot 2022-10-23 at 22 51 14" src="https://user-images.githubusercontent.com/54627841/197439210-605a69b8-24ea-48e2-8b5f-e479d11a2bcb.png"><br>
      6. The above screenshot shows a hash of 75 with the actor's finger table. It will hop to the 68th NodeID as 68 is the greatest successor, less than 75. The actor will send a message to the actor corresponding to 68 using PID <0.135.0>
      7. A counter is maintained for each actor to calculate the hops required. Average Hops are calculated by dividing total interactions and are output on the console for each run.<br>
<img width="293" alt="Screenshot 2022-10-23 at 23 04 03" src="https://user-images.githubusercontent.com/54627841/197440550-ddfecf38-9ae5-4f0f-8dd8-7a53ab2f94a0.png"> <br>


2. The **largest network** we managed to deal with is **2000 nodes for 1000 NumRequests**. 
3. We used Macbook Air M1 and Lenovo Legion Intel i7 10 generations to run the protocol.
4. Observation was as the number of nodes increased, the average hops increased.<br>
![Screenshot 2022-10-23 at 23 30 27](https://user-images.githubusercontent.com/54627841/197443253-b0e2ea06-7e38-4505-90ca-70022b7bc048.png)




