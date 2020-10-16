with Primes.PrimeNumberRequest_DataWriter;
with Primes.PrimeNumberReply_DataReader;
with DDS.Request_Reply.Requester.Typed_Requester_Generic;
package Primes.PrimeNumberRequester is new DDS.Request_Reply.Requester.Typed_Requester_Generic
  (Request_DataWriter => Primes.PrimeNumberRequest_DataWriter,
   Reply_DataReader   => Primes.PrimeNumberReply_DataReader);

