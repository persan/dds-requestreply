with Primes.PrimeNumberRequest_DataReader;
with Primes.PrimeNumberReply_DataWriter;
with DDS.Typed_Replyer_Generic;
package Primes.PrimeNumberReplier is new DDS.Typed_Replyer_Generic
  (Request_DataReaders => PrimeNumberRequest_DataReader,
   Reply_DataWriters   => PrimeNumberReply_DataWriter);
