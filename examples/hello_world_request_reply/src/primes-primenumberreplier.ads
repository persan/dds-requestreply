with Primes.PrimeNumberRequest_DataReader;
with Primes.PrimeNumberReply_DataWriter;
with DDS.Request_Reply.Replier.Typed_Replier_Generic;
package Primes.PrimeNumberReplier is new DDS.Request_Reply.Replier.Typed_Replier_Generic
  (Request_DataReader => Primes.PrimeNumberRequest_DataReader,
   Reply_DataWriter   => Primes.PrimeNumberReply_DataWriter);
