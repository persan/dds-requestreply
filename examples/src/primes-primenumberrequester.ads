with Primes.PrimeNumberRequest_DataWriter;
with Primes.PrimeNumberReply_DataReader;
with DDS.Typed_Requester_Generic;
package Primes.PrimeNumberRequester is new DDS.Typed_Requester_Generic
  (Request_DataWriters => Primes.PrimeNumberRequest_DataWriter,
   Reply_DataReaders   => Primes.PrimeNumberReply_DataReader);

