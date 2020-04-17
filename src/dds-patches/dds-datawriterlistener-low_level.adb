--  (c) Copyright, Real-Time Innovations, $Date:: 2012-10-23 #$
--  All rights reserved.
--
--  No duplications, whole or partial, manual or electronic, may be made
--  without express written permission.  Any such copies, or
--  revisions thereof, must display this notice unaltered.
--  This code contains trade secrets of Real-Time Innovations, Inc.

pragma Ada_2012;

with DDS.DataWriter;
with DDS.DataWriter_Impl;
with DDS.Internal_Conversions;
with ADA.Unchecked_Conversion;

package body DDS.DataWriterListener.Low_Level is
   use DDS.Internal_Conversions;
   function Convert is new Ada.Unchecked_Conversion (System.Address, DDS.DataWriterListener.Ref_Access);

   procedure On_Offered_Deadline_Missed
     (Listener : System.Address;
      C_Writer : System.Address;
      Status   : access constant DDS_OfferedDeadlineMissedStatus) is
      Writer : DDS.DataWriter.Ref_Access;
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Offered_Deadline_Missed (Writer, Convert (Status.all));
   end On_Offered_Deadline_Missed;

   procedure On_Offered_Incompatible_Qos
     (Listener : System.Address;
      C_Writer : System.Address;
      Status   : access constant DDS_OfferedIncompatibleQosStatus) is
      Writer : DDS.DataWriter.Ref_Access;
      type DDS_OfferedIncompatibleQosStatus_Access is access DDS_OfferedIncompatibleQosStatus;
      function Convert is new Ada.Unchecked_Conversion (DDS_OfferedIncompatibleQosStatus_Access,
                                                        OfferedIncompatibleQosStatus_Access);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Offered_Incompatible_Qos (Writer, Convert (Status.all'Unrestricted_Access).all);
   end On_Offered_Incompatible_Qos;

   procedure On_Liveliness_Lost
     (Listener : System.Address;
      C_Writer : System.Address;
      Status   : access constant DDS_LivelinessLostStatus) is
      Writer : DDS.DataWriter.Ref_Access;
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Liveliness_Lost (Writer, Convert (Status.all));
   end On_Liveliness_Lost;

   procedure On_Publication_Matched
     (Listener : System.Address;
      C_Writer : System.Address;
      Status   : access constant DDS_PublicationMatchedStatus) is
      Writer : DDS.DataWriter.Ref_Access;
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Publication_Matched (Writer, Convert (Status.all));
   end On_Publication_Matched;

   procedure On_Reliable_Writer_Cache_Changed
     (Listener : System.Address;
      C_Writer : System.Address;
      Status   : access constant DDS_ReliableWriterCacheChangedStatus) is
      Writer : DDS.DataWriter.Ref_Access;
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Reliable_Writer_Cache_Changed (Writer, Convert (Status.all));
   end On_Reliable_Writer_Cache_Changed;

   procedure On_Reliable_Reader_Activity_Changed
     (Listener : System.Address;
      C_Writer : System.Address;
      Status   : access constant DDS_ReliableReaderActivityChangedStatus) is
      Writer : DDS.DataWriter.Ref_Access;
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Reliable_Reader_Activity_Changed (Writer, Convert (Status.all));
   end On_Reliable_Reader_Activity_Changed;

   procedure On_Destination_Unreachable
     (Listener : System.Address;
      C_Writer : System.Address;
      Instance : access constant RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
      Locator  : access constant RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_Locator_T) is
      Writer : DDS.DataWriter.Ref_Access;
      function ConvertInstance is new Ada.Unchecked_Conversion
        (RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle,
         DDS.InstanceHandle_T);
      function ConvertLocator is new Ada.Unchecked_Conversion
        (RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_Locator_T,
         DDS.Locator_T);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Destination_Unreachable (Writer, ConvertInstance (Instance.all), ConvertLocator (Locator.all));
   end On_Destination_Unreachable;

   function On_Data_Request
     (Listener : System.Address;
      C_Writer : System.Address;
      Cookie   : access constant RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_Cookie_T)
      return System.Address is
      Writer : DDS.DataWriter.Ref_Access;
      Value  : aliased System.Address;
      function ConvertCookie is new Ada.Unchecked_Conversion (RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_Cookie_T,
                                                              DDS.Cookie_T);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Data_Request (Writer, ConvertCookie (Cookie.all), Value);
      return Value;
   end On_Data_Request;

   procedure On_Data_Return
     (Listener : System.Address;
      C_Writer : System.Address;
      Arg      : System.Address;
      Cookie   : access constant RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_Cookie_T) is
      Writer : DDS.DataWriter.Ref_Access;
      function ConvertCookie is new Ada.Unchecked_Conversion (RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_Cookie_T,
                                                              DDS.Cookie_T);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Data_Return (Writer, Arg, ConvertCookie (Cookie.all));
   end On_Data_Return;

   procedure On_Sample_Removed
     (Listener : System.Address;
      C_Writer : System.Address;
      Cookie   : access constant RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_Cookie_T) is
      Writer : DDS.DataWriter.Ref_Access;
      function ConvertCookie is new Ada.Unchecked_Conversion (RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_Cookie_T,
                                                              DDS.Cookie_T);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Sample_Removed (Writer, ConvertCookie (Cookie.all));
   end On_Sample_Removed;

   procedure On_Instance_Replaced
     (Listener : System.Address;
      C_Writer : System.Address;
      Instance : access constant RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle) is
      Writer : DDS.DataWriter.Ref_Access;
      function ConvertInstance is new Ada.Unchecked_Conversion (RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle,
                                                                DDS.InstanceHandle_T);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Instance_Replaced (Writer, ConvertInstance (Instance.all));
   end On_Instance_Replaced;

   procedure On_Application_Acknowledgment
     (Listener : System.Address;
      C_Writer : System.Address;
      Info     : access constant DDS_AcknowledgmentInfo) is
      Writer : DDS.DataWriter.Ref_Access;
      type DDS_AcknowledgmentInfo_Access is access all DDS_AcknowledgmentInfo with Storage_Size => 0;
      function Convert is new Ada.Unchecked_Conversion (Source => System.Address,
                                                        Target => DDS.AcknowledgmentInfo_Access);
   begin
      Writer := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));
      Convert (Listener).On_Application_Acknowledgment (Writer, Convert (Info.all'Address).all);
   end On_Application_Acknowledgment;

   procedure On_Service_Request_Accepted
     (Listener : System.Address;
      C_Writer : System.Address;
      Info     : access constant DDS_ServiceRequestAcceptedStatus) is

      Writer : constant DDS.DataWriter.Ref_Access := DDS.DataWriter.Ref_Access
        (DDS.DataWriter_Impl.Get_FacadeI (C_Writer));

      type DDS_ServiceRequestAcceptedStatus_Access is access all DDS_ServiceRequestAcceptedStatus with Storage_Size => 0;
      function Convert is new Ada.Unchecked_Conversion (Source => System.Address,
                                                        Target => DDS.ServiceRequestAcceptedStatus_Access);
   begin
      Convert (Listener).On_Service_Request_Accepted (Writer, Convert (Info.all'Address).all);
   end On_Service_Request_Accepted;

end DDS.DataWriterListener.Low_Level;
