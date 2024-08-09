-- ---------------------------------------------------------------------
--                                                                    --
--               Copyright (c) per.sandberg@bahnhof.se                --
--                                                                    --
--  Permission is hereby granted, free of charge, to any person       --
--  obtaining a copy of this software and associated documentation    --
--  files (the "Software"), to deal in the Software without           --
--  restriction, including without limitation the rights to use,      --
--  copy, modify, merge, publish, distribute, sublicense, and/or sell --
--  copies of the Software, and to permit persons to whom the Software--
--  is furnished to do so, subject to the following conditions:       --
--                                                                    --
--  The above copyright notice and this permission notice             --
--  (including the next paragraph) shall be included in all copies or --
--  substantial portions of the Software.                             --
--                                                                    --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   --
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF--
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND             --
--  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT       --
--  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,      --
--  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,--
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER     --
--  DEALINGS IN THE SOFTWARE.                                         --
--                                                                    --
--  <spdx: MIT>
--                                                                    --
-- ---------------------------------------------------------------------

pragma Warnings (Off);
separate (DDS.Request_Reply.Requester.Typed_Requester_Generic)
package body Listners_Impl is
--------------------------------
-- On_Offered_Deadline_Missed --
--------------------------------

   procedure On_Offered_Deadline_Missed
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.OfferedDeadlineMissedStatus)
   is
   begin
      Self.Parent.Listner.On_Offered_Deadline_Missed (Typed_Requester_Generic.Ref_Access (Writer), Status);
   end On_Offered_Deadline_Missed;

   -----------------------
   -- On_Data_Available --
   -----------------------

   procedure On_Data_Available
     (Self       :    not null access DataReader_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access)
   is
   begin
      -- Self.Parent.Listner.On_Data_Available (The_Reader);
      null;
   end On_Data_Available;

   ---------------------------------
   -- On_Offered_Incompatible_Qos --
   ---------------------------------

   procedure On_Offered_Incompatible_Qos
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.OfferedIncompatibleQosStatus)
   is
   begin
      Self.Parent.Listner.On_Offered_Incompatible_Qos (Typed_Requester_Generic.Ref_Access (Writer), Status);
   end On_Offered_Incompatible_Qos;

   ------------------------
   -- On_Liveliness_Lost --
   ------------------------

   procedure On_Liveliness_Lost
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.LivelinessLostStatus)
   is
   begin
      Self.Parent.Listner.On_Liveliness_Lost (Typed_Requester_Generic.Ref_Access (Writer), Status);
   end On_Liveliness_Lost;

   ----------------------------
   -- On_Publication_Matched --
   ----------------------------

   procedure On_Publication_Matched
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.PublicationMatchedStatus)
   is
   begin
      Self.Parent.Listner.On_Publication_Matched (Typed_Requester_Generic.Ref_Access (Writer), Status);
   end On_Publication_Matched;

   --------------------------------------
   -- On_Reliable_Writer_Cache_Changed --
   --------------------------------------

   procedure On_Reliable_Writer_Cache_Changed
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.ReliableWriterCacheChangedStatus)
   is
   begin
      Self.Parent.Listner.On_Reliable_Writer_Cache_Changed (Typed_Requester_Generic.Ref_Access (Writer), Status);
   end On_Reliable_Writer_Cache_Changed;

   -----------------------------------------
   -- On_Reliable_Reader_Activity_Changed --
   -----------------------------------------

   procedure On_Reliable_Reader_Activity_Changed
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.ReliableReaderActivityChangedStatus)
   is
   begin
      Self.Parent.Listner.On_Reliable_Reader_Activity_Changed (Typed_Requester_Generic.Ref_Access (Writer), Status);
   end On_Reliable_Reader_Activity_Changed;

   --------------------------------
   -- On_Destination_Unreachable --
   --------------------------------

   procedure On_Destination_Unreachable
     (Self     :    not null access DataReader_Listner;
      Writer   :    access DDS.DataWriter.Ref'Class;
      Instance : in DDS.InstanceHandle_T;
      Locator  : in DDS.Locator_T)
   is
   begin
      Self.Parent.Listner.On_Destination_Unreachable (Typed_Requester_Generic.Ref_Access (Writer), Instance, Locator);
   end On_Destination_Unreachable;

   ---------------------
   -- On_Data_Request --
   ---------------------

   procedure On_Data_Request
     (Self    : not null access DataReader_Listner;
      Writer  : access DDS.DataWriter.Ref'Class;
      Cookie  : in DDS.Cookie_T;
      Request : in out System.Address)
   is
   begin
      Self.Parent.Listner.On_Data_Request (Typed_Requester_Generic.Ref_Access (Writer), Cookie, Request);
   end On_Data_Request;

   --------------------
   -- On_Data_Return --
   --------------------

   procedure On_Data_Return
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Arg    : System.Address;
      Cookie : in DDS.Cookie_T)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "On_Data_Return unimplemented");
      raise Program_Error with "Unimplemented procedure On_Data_Return";
   end On_Data_Return;

   -----------------------
   -- On_Sample_Removed --
   -----------------------

   procedure On_Sample_Removed
     (Self   : not null access DataReader_Listner;
      Writer : access DDS.DataWriter.Ref'Class;
      Cookie : in DDS.Cookie_T)
   is
   begin
      Self.Parent.Listner.On_Sample_Removed (Typed_Requester_Generic.Ref_Access (Writer), Cookie);
   end On_Sample_Removed;

   --------------------------
   -- On_Instance_Replaced --
   --------------------------

   procedure On_Instance_Replaced
     (Self     :    not null access DataReader_Listner;
      Writer   :    access DDS.DataWriter.Ref'Class;
      Instance : in DDS.InstanceHandle_T)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "On_Instance_Replaced unimplemented");
      raise Program_Error with "Unimplemented procedure On_Instance_Replaced";
   end On_Instance_Replaced;

   -----------------------------------
   -- On_Application_Acknowledgment --
   -----------------------------------

   procedure On_Application_Acknowledgment
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Info   : in DDS.AcknowledgmentInfo)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "On_Application_Acknowledgment unimplemented");
      raise Program_Error with "Unimplemented procedure On_Application_Acknowledgment";
   end On_Application_Acknowledgment;

   ---------------------------------
   -- On_Service_Request_Accepted --
   ---------------------------------

   procedure On_Service_Request_Accepted
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Info   : in DDS.ServiceRequestAcceptedStatus)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "On_Service_Request_Accepted unimplemented");
      raise Program_Error with "Unimplemented procedure On_Service_Request_Accepted";
   end On_Service_Request_Accepted;

   ----------------------------------
   -- On_Requested_Deadline_Missed --
   ----------------------------------

   procedure On_Requested_Deadline_Missed
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.RequestedDeadlineMissedStatus)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "On_Requested_Deadline_Missed unimplemented");
      raise Program_Error
        with "Unimplemented procedure On_Requested_Deadline_Missed";
   end On_Requested_Deadline_Missed;

   -----------------------------------
   -- On_Requested_Incompatible_Qos --
   -----------------------------------

   procedure On_Requested_Incompatible_Qos
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.RequestedIncompatibleQosStatus)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "On_Requested_Incompatible_Qos unimplemented");
      raise Program_Error
        with "Unimplemented procedure On_Requested_Incompatible_Qos";
   end On_Requested_Incompatible_Qos;

   ------------------------
   -- On_Sample_Rejected --
   ------------------------

   procedure On_Sample_Rejected
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.SampleRejectedStatus)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "On_Sample_Rejected unimplemented");
      raise Program_Error with "Unimplemented procedure On_Sample_Rejected";
   end On_Sample_Rejected;

   ---------------------------
   -- On_Liveliness_Changed --
   ---------------------------

   procedure On_Liveliness_Changed
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.LivelinessChangedStatus)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "On_Liveliness_Changed unimplemented");
      raise Program_Error with "Unimplemented procedure On_Liveliness_Changed";
   end On_Liveliness_Changed;

   -----------------------
   -- On_Data_Available --
   -----------------------

   procedure On_Data_Available
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "On_Data_Available unimplemented");
      raise Program_Error with "Unimplemented procedure On_Data_Available";
   end On_Data_Available;

   -----------------------------
   -- On_Subscription_Matched --
   -----------------------------

   procedure On_Subscription_Matched
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.SubscriptionMatchedStatus)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "On_Subscription_Matched unimplemented");
      raise Program_Error with "Unimplemented procedure On_Subscription_Matched";
   end On_Subscription_Matched;

   --------------------
   -- On_Sample_Lost --
   --------------------

   procedure On_Sample_Lost
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.SampleLostStatus)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "On_Sample_Lost unimplemented");
      raise Program_Error with "Unimplemented procedure On_Sample_Lost";
   end On_Sample_Lost;

end Listners_Impl;
