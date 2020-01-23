with Primes.PrimeNumberRequest_DataWriter;
with Primes.PrimeNumberReply_DataReader;
with DDS.Typed_Requester_Generic;
package Primes.PrimeNumberRequester is new DDS.Typed_Requester_Generic
  (Request_DataWriters => PrimeNumberRequest_DataWriter,
   Reply_DataReaders   => PrimeNumberReply_DataReader);

