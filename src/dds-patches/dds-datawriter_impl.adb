--  (c) Copyright, Real-Time Innovations, $Date:: 2012-12-03 #$
--  All rights reserved.
--
--  No duplications, whole or partial, manual or electronic, may be made
--  without express written permission.  Any such copies, or
--  revisions thereof, must display this notice unaltered.
--  This code contains trade secrets of Real-Time Innovations, Inc.

pragma Ada_12;


with Ada.Unchecked_Conversion;
with Interfaces.C;

with DDS.Publisher_Impl;
with DDS.Topic_Impl;
with DDS.TypeSupport;
with DDS.DomainParticipantFactory;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Common_H;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Builtin_H;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Publication_H;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Publication_Impl_H;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Topic_H;
with RTIDDS.Low_Level.Ndds_Pres_Pres_TypePlugin_H;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_Impl_H;
with RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H;
with RTIDDS.Obj_Impl;
with DDS.DataWriterListener.Low_Level;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Domain_Impl_H;
pragma Unreferenced (RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Domain_Impl_H);
--  Domain_H only has to be included because of CORE-6744. Remove once this is fixed
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Domain_H;
with RTIDDS.Low_Level.Ndds_Pres_Pres_Common_H;

package body DDS.DataWriter_Impl is

   use Interfaces;
   use Interfaces.C;


   --  Domain_H only has to be included because of CORE-6744. Remove once this is fixed
   use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Domain_H;
   use RTIDDS.Low_Level.Ndds_Pres_Pres_Common_H;
   use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Builtin_H;
   use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H;
   use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_Impl_H;
   use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Publication_Impl_H;
   use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Publication_H;
   use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Topic_H;

   use type DDS.TypeSupport.Ref_Access;
   use type System.Address;


   type Ref_Access_Access is access all Ref_Access;
   for Ref_Access_Access'Storage_Size use 0;
   function Convert is new Ada.Unchecked_Conversion (System.Address, Ref_Access_Access);
   function Convert is new Ada.Unchecked_Conversion (System.Address, DDS.DataWriterListener.Ref_Access);

   -------------
   -- Set_Qos --
   -------------
   procedure Set_Qos
     (Self : not null access Ref;
      Qos  : in DDS.DataWriterQos) is

   begin
      Ret_Code_To_Exception (DDS_DataWriter_Set_Qos (Self.GetInterface, GetInterface (Qos)));
   end Set_Qos;

   procedure  Set_Qos_With_Profile (Self         : not null access Ref;
                                    Library_Name : String;
                                    Profile_Name : String) is
      Ret : RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_ReturnCode_T;
   begin
      Ret := DDS_DataWriter_Set_Qos_With_Profile
        (Self.GetInterface,
         GetInterface (Library_Name),
         GetInterface (Profile_Name));
      Ret_Code_To_Exception
        (Ret, To_Standard_String (Library_Name) & "::"
         & To_Standard_String (Profile_Name));
   end Set_Qos_With_Profile;

   procedure  Set_Qos_With_Profile (Self         : not null access Ref;
                                    Library_Name : Standard.String;
                                    Profile_Name : Standard.String) is
      L   : Interfaces.C.Strings.chars_ptr := New_String (Library_Name);
      P   : Interfaces.C.Strings.chars_ptr := New_String (Profile_Name);
      Ret : RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_ReturnCode_T;
   begin
      Ret := DDS_DataWriter_Set_Qos_With_Profile
        (Self.GetInterface,
         L,
         P);
      Free (L);
      Free (P);
      Ret_Code_To_Exception  (Ret, Library_Name & "::" & Profile_Name);
   end Set_Qos_With_Profile;
   -------------
   -- Get_Qos --
   -------------
   procedure Get_Qos
     (Self : not null access Ref;
      Qos  : in out DDS.DataWriterQos) is

   begin
      Ret_Code_To_Exception (DDS_DataWriter_Get_Qos (Self.GetInterface, GetInterface (Qos)));
   end Get_Qos;


   ------------------
   -- Set_Listener --
   ------------------

   procedure Set_Listener
     (Self       : not null access Ref;
      A_Listener : in DDS.DataWriterListener.Ref_Access;
      Mask       : in DDS.StatusMask)
   is
      C_Listener : aliased DDS_DataWriterListener :=
                     DDS.DataWriterListener.Low_Level.DataWriterListener_DEFAULT;
      function Convert is new Ada.Unchecked_Conversion (DDS.DataWriterListener.Ref_Access, System.Address);
      use type DDS.DataWriterListener.Ref_Access;
   begin
      if A_Listener /= null then
         C_Listener.As_Listener.Listener_Data := Convert (A_Listener);

         Ret_Code_To_Exception (
                                DDS_DataWriter_Set_Listener (Self.GetInterface,
                                  C_Listener'Access,
                                  DDS_StatusMask (Mask)),
                                "Set Listener FAILED");
      else
         Ret_Code_To_Exception (
                                DDS_DataWriter_Set_Listener (Self.GetInterface,
                                  null,
                                  DDS_StatusMask (Mask)),
                                "Set Listener FAILED");
      end if;
   end Set_Listener;

   ------------------
   -- Get_Listener --
   ------------------
   function Get_Listener
     (Self : not null access Ref)
      return DDS.DataWriterListener.Ref_Access is

      C_Listener : aliased DDS_DataWriterListener;

   begin
      Ret_Code_To_Exception (DDS_DataWriter_Get_ListenerX (Self.GetInterface, C_Listener'Access));
      return Convert (C_Listener.As_Listener.Listener_Data);
   end Get_Listener;

   ---------------
   -- Get_Topic --
   ---------------

   function Get_Topic
     (Self : not null access Ref)
      return DDS.Topic.Ref_Access
   is
      C_TopicW  : constant access RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Topic_H.DDS_Topic := DDS_DataWriter_Get_Topic (Self.GetInterface);
      Topic     : DDS.Topic_Impl.Ref_Access;
   begin
      if C_TopicW /= null then
         Topic := DDS.Topic_Impl.Get_FacadeI (C_TopicW);

         return DDS.Topic.Ref_Access (Topic);
      end if;

      return null;
   end Get_Topic;

   -------------------
   -- Get_Publisher --
   -------------------

   function Get_Publisher
     (Self : not null access Ref)
      return access DDS.Publisher.Ref'Class
   is
      C_Publisher : constant System.Address := DDS_DataWriter_Get_Publisher (Self.GetInterface);
   begin
      if C_Publisher /= System.Null_Address then
         return DDS.Publisher_Impl.Get_FacadeI (C_Publisher);
      end if;
      return null;
   end Get_Publisher;

   ------------------------------
   -- Wait_For_Acknowledgments --
   ------------------------------

   procedure Wait_For_Acknowledgments
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
   is
      Max_Wait_Local : aliased constant DDS_Duration_T := (C.int (Max_Wait.Sec), C.Unsigned (Max_Wait.Nanosec));
   begin
      Ret_Code_To_Exception (DDS_DataWriter_Wait_For_Acknowledgments (Self.GetInterface, Max_Wait_Local'Access));
   end Wait_For_Acknowledgments;

   --------------------------------------
   -- Wait_For_Asynchronous_Publishing --
   --------------------------------------

   procedure Wait_For_Asynchronous_Publishing
     (Self     : not null access Ref;
      Max_Wait : in DDS.Duration_T)
   is
      Max_Wait_Local : aliased constant DDS_Duration_T := (C.int (Max_Wait.Sec), C.Unsigned (Max_Wait.Nanosec));
   begin
      Ret_Code_To_Exception (
                             DDS_DataWriter_Wait_For_Asynchronous_Publishing (
                               Self.GetInterface, Max_Wait_Local'Access));
   end Wait_For_Asynchronous_Publishing;

   --------------------------------
   -- Get_Liveliness_Lost_Status --
   --------------------------------

   procedure Get_Liveliness_Lost_Status
     (Self   : not null access Ref;
      Status : in out DDS.LivelinessLostStatus)
   is
      type DDS_LivelinessLostStatus_Access is access all DDS_LivelinessLostStatus;
      type LivelinessLostStatus_Access is access all DDS.LivelinessLostStatus;
      function Convert is new Ada.Unchecked_Conversion (LivelinessLostStatus_Access, DDS_LivelinessLostStatus_Access);
      LowLevelStatus : constant access DDS_LivelinessLostStatus := Convert (Status'Unrestricted_Access);
   begin
      Ret_Code_To_Exception (DDS_DataWriter_Get_Liveliness_Lost_Status (Self.GetInterface, LowLevelStatus));
   end Get_Liveliness_Lost_Status;

   ----------------------------------------
   -- Get_Offered_Deadline_Missed_Status --
   ----------------------------------------

   procedure Get_Offered_Deadline_Missed_Status
     (Self   : not null access Ref;
      Status : in out DDS.OfferedDeadlineMissedStatus)
   is
      type DDS_OfferedDeadlineMissedStatus_Access is access all DDS_OfferedDeadlineMissedStatus;
      type OfferedDeadlineMissedStatus_Access is access all DDS.OfferedDeadlineMissedStatus;
      function Convert is new Ada.Unchecked_Conversion (OfferedDeadlineMissedStatus_Access, DDS_OfferedDeadlineMissedStatus_Access);
      LowLevelStatus : constant access DDS_OfferedDeadlineMissedStatus := Convert (Status'Unrestricted_Access);
   begin
      Ret_Code_To_Exception (DDS_DataWriter_Get_Offered_Deadline_Missed_Status (Self.GetInterface, LowLevelStatus));
   end Get_Offered_Deadline_Missed_Status;

   -----------------------------------------
   -- Get_Offered_Incompatible_Qos_Status --
   -----------------------------------------

   procedure Get_Offered_Incompatible_Qos_Status
     (Self   : not null access Ref;
      Status : in out DDS.OfferedIncompatibleQosStatus)
   is
      type DDS_OfferedIncompatibleQosStatus_Access is access all DDS_OfferedIncompatibleQosStatus;
      type OfferedIncompatibleQosStatus_Access is access all DDS.OfferedIncompatibleQosStatus;
      function Convert is new Ada.Unchecked_Conversion (OfferedIncompatibleQosStatus_Access, DDS_OfferedIncompatibleQosStatus_Access);
      LowLevelStatus : constant access DDS_OfferedIncompatibleQosStatus := Convert (Status'Unrestricted_Access);
   begin
      Ret_Code_To_Exception (DDS_DataWriter_Get_Offered_Incompatible_Qos_Status (Self.GetInterface, LowLevelStatus));
   end Get_Offered_Incompatible_Qos_Status;

   ------------------------------------
   -- Get_Publication_Matched_Status --
   ------------------------------------

   procedure Get_Publication_Matched_Status
     (Self   : not null access Ref;
      Status : in out DDS.PublicationMatchedStatus)
   is
      type DDS_PublicationMatchedStatus_Access is access all DDS_PublicationMatchedStatus;
      type PublicationMatchedStatus_Access is access all DDS.PublicationMatchedStatus;
      function Convert is new Ada.Unchecked_Conversion (PublicationMatchedStatus_Access, DDS_PublicationMatchedStatus_Access);
      LowLevelStatus : constant access DDS_PublicationMatchedStatus := Convert (Status'Unrestricted_Access);
   begin
      Ret_Code_To_Exception (DDS_DataWriter_Get_Publication_Matched_Status (Self.GetInterface, LowLevelStatus));
   end Get_Publication_Matched_Status;

   -----------------------
   -- Assert_Liveliness --
   -----------------------

   procedure Assert_Liveliness
     (Self : not null access Ref)
   is
   begin
      Ret_Code_To_Exception (DDS_DataWriter_Assert_Liveliness (Self.GetInterface));
   end Assert_Liveliness;

   ------------------------------
   -- Get_Matched_Subscriptions --
   ------------------------------

   type DDS_InstanceHandleSeq_Access is access all DDS_InstanceHandleSeq;
   function Get_Matched_Subscriptions
     (Self    : not null access Ref)
      return DDS.InstanceHandle_Seq.Sequence
   is
      Arg : access DDS_InstanceHandleSeq;
      function Convert is new Ada.Unchecked_Conversion (DDS.InstanceHandle_Seq.Sequence_Access, DDS_InstanceHandleSeq_Access);
   begin
      return Ret : DDS.InstanceHandle_Seq.Sequence do
         Arg := Convert (Ret'Unrestricted_Access);
         Ret_Code_To_Exception (DDS_DataWriter_Get_Matched_Subscriptions (Self.GetInterface, Arg));

      end return;
   end Get_Matched_Subscriptions;

   -----------------------------------
   -- Get_Matched_Subscription_Data --
   -----------------------------------

   function Get_Matched_Subscription_Data
     (Self                : not null access Ref;
      Subscription_Handle : in DDS.InstanceHandle_T)
      return DDS.SubscriptionBuiltinTopicData
   is

      Subscription_Handle_Local : aliased constant DDS_InstanceHandle_T :=
                                    DDS_InstanceHandle_T (Subscription_Handle);
      type DDS_SubscriptionBuiltinTopicData_Access is access all DDS_SubscriptionBuiltinTopicData;
      type SubscriptionBuiltinTopicData_Access is access all SubscriptionBuiltinTopicData;
      function Convert is new Ada.Unchecked_Conversion (SubscriptionBuiltinTopicData_Access,
                                                        DDS_SubscriptionBuiltinTopicData_Access);
   begin
      return Ret : DDS.SubscriptionBuiltinTopicData do
         Ret_Code_To_Exception
           (DDS_DataWriter_Get_Matched_Subscription_Data
              (Self.GetInterface,
               Convert (Ret'Unrestricted_Access),
               Subscription_Handle_Local'Access));
      end return;
   end Get_Matched_Subscription_Data;

   ---------------------------------------
   -- Get_Matched_Subscription_Locators --
   ---------------------------------------

   function Get_Matched_Subscription_Locators
     (Self    : not null access Ref)
      return DDS.Locator_Seq.Sequence
   is
      Arg : access DDS_LocatorSeq;
      type DDS_LocatorSeq_Access is access all DDS_LocatorSeq;
      type Locator_Seq_Access is access all DDS.Locator_Seq.Sequence;
      function Convert is new Ada.Unchecked_Conversion
        (Locator_Seq_Access, DDS_LocatorSeq_Access);
   begin
      return Ret : DDS.Locator_Seq.Sequence do
         Arg := Convert (Ret'Unrestricted_Access);
         Ret_Code_To_Exception
           (DDS_DataWriter_Get_Matched_Subscription_Locators
              (Self.GetInterface, Arg));
      end return;
   end Get_Matched_Subscription_Locators;

   -------------------------------------------------
   -- Get_Reliable_Reader_Activity_Changed_Status --
   -------------------------------------------------

   procedure Get_Reliable_Reader_Activity_Changed_Status
     (Self   : not null access Ref;
      Status : in out DDS.ReliableReaderActivityChangedStatus)
   is
      type DDS_ReliableReaderActivityChangedStatus_Access is access all DDS_ReliableReaderActivityChangedStatus;
      type ReliableReaderActivityChangedStatus_Access is access all DDS.ReliableReaderActivityChangedStatus;
      function Convert is new Ada.Unchecked_Conversion
        (ReliableReaderActivityChangedStatus_Access,
         DDS_ReliableReaderActivityChangedStatus_Access);
      Arg : access DDS_ReliableReaderActivityChangedStatus;
   begin
      Arg := Convert (Status'Unrestricted_Access);
      Ret_Code_To_Exception
        (DDS_DataWriter_Get_Reliable_Reader_Activity_Changed_Status
           (Self.GetInterface, Arg));
   end Get_Reliable_Reader_Activity_Changed_Status;

   ----------------------------------------------
   -- Get_Reliable_Writer_Cache_Changed_Status --
   ----------------------------------------------

   procedure Get_Reliable_Writer_Cache_Changed_Status
     (Self   : not null access Ref;
      Status : in out DDS.ReliableWriterCacheChangedStatus)
   is
      type DDS_ReliableWriterCacheChangedStatus_Access is access all DDS_ReliableWriterCacheChangedStatus;
      type ReliableWriterCacheChangedStatus_Access is access all DDS.ReliableWriterCacheChangedStatus;
      function Convert is new Ada.Unchecked_Conversion
        (ReliableWriterCacheChangedStatus_Access,
         DDS_ReliableWriterCacheChangedStatus_Access);
      Arg : access DDS_ReliableWriterCacheChangedStatus;
   begin
      Arg := Convert (Status'Unrestricted_Access);
      Ret_Code_To_Exception
        (DDS_DataWriter_Get_Reliable_Writer_Cache_Changed_Status
           (Self.GetInterface, Arg));
   end Get_Reliable_Writer_Cache_Changed_Status;

   ---------------------------------
   -- Get_DataWriter_Cache_Status --
   ---------------------------------

   procedure Get_DataWriter_Cache_Status
     (Self   : not null access Ref;
      Status : in out DDS.DataWriterCacheStatus)
   is
      type DDS_DataWriterCacheStatus_Access is access all DDS_DataWriterCacheStatus;
      type DataWriterCacheStatus_Access is access all DDS.DataWriterCacheStatus;
      function Convert is new Ada.Unchecked_Conversion
        (DataWriterCacheStatus_Access,
         DDS_DataWriterCacheStatus_Access);
      Arg : access DDS_DataWriterCacheStatus;
   begin
      Arg := Convert (Status'Unrestricted_Access);
      Ret_Code_To_Exception
        (DDS_DataWriter_Get_Datawriter_Cache_Status
           (Self.GetInterface, Arg));
   end Get_DataWriter_Cache_Status;

   ---------------------------------
   -- Get_DataWriter_Protocol_Status --
   ---------------------------------

   procedure Get_DataWriter_Protocol_Status
     (Self   : not null access Ref;
      Status : in out DDS.DataWriterProtocolStatus)
   is
      type DDS_DataWriterProtocolStatus_Access is access all DDS_DataWriterProtocolStatus;
      type DataWriterProtocolStatus_Access is access all DDS.DataWriterProtocolStatus;
      function Convert is new Ada.Unchecked_Conversion
        (DataWriterProtocolStatus_Access,
         DDS_DataWriterProtocolStatus_Access);
      Arg : access DDS_DataWriterProtocolStatus;
   begin
      Arg := Convert (Status'Unrestricted_Access);
      Ret_Code_To_Exception
        (DDS_DataWriter_Get_Datawriter_Protocol_Status
           (Self.GetInterface, Arg));
   end Get_DataWriter_Protocol_Status;

   ---------------------------------------------------------
   -- Get_Matched_Subscription_Datawriter_Protocol_Status --
   ---------------------------------------------------------

   procedure Get_Matched_Subscription_Datawriter_Protocol_Status
     (Self                : not null access Ref;
      Status              : in out DDS.DataWriterProtocolStatus;
      Subscription_Handle : DDS.InstanceHandle_T)
   is
      type DDS_DataWriterProtocolStatus_Access is access all DDS_DataWriterProtocolStatus;
      type DataWriterProtocolStatus_Access is access all DDS.DataWriterProtocolStatus;
      function ConvertPS is new Ada.Unchecked_Conversion
        (DataWriterProtocolStatus_Access,
         DDS_DataWriterProtocolStatus_Access);
      type DDS_InstanceHandle_T_Access is access all DDS_InstanceHandle_T;
      function ConvertIH is new Ada.Unchecked_Conversion
        (DDS.InstanceHandle_T_Access,
         DDS_InstanceHandle_T_Access);
      Arg1 : access DDS_DataWriterProtocolStatus;
      Arg2 : access DDS_InstanceHandle_T;
   begin
      Arg1 := ConvertPS (Status'Unrestricted_Access);
      Arg2 := ConvertIH (Subscription_Handle'Unrestricted_Access);
      Ret_Code_To_Exception
        (DDS_DataWriter_Get_Matched_Subscription_Datawriter_Protocol_Status
           (Self.GetInterface, Arg1, Arg2));
   end Get_Matched_Subscription_Datawriter_Protocol_Status;

   --------------------------------------------------------------------
   -- Get_Matched_Subscription_Datawriter_Protocol_Status_By_Locator --
   --------------------------------------------------------------------

   procedure Get_Matched_Subscription_Datawriter_Protocol_Status_By_Locator
     (Self    : not null access Ref;
      Status  : in out DDS.DataWriterProtocolStatus;
      Locator : DDS.Locator_T)
   is
      type DDS_DataWriterProtocolStatus_Access is access all DDS_DataWriterProtocolStatus;
      type DataWriterProtocolStatus_Access is access all DDS.DataWriterProtocolStatus;
      function ConvertPS is new Ada.Unchecked_Conversion
        (DataWriterProtocolStatus_Access,
         DDS_DataWriterProtocolStatus_Access);
      type DDS_LocaTOR_T_Access is access all DDS_Locator_T;
      function ConvertLoc is new Ada.Unchecked_Conversion
        (DDS.Locator_T_Access,
         DDS_LocaTOR_T_Access);
      Arg1 : access DDS_DataWriterProtocolStatus;
      Arg2 : access DDS_Locator_T;
   begin
      Arg1 := ConvertPS (Status'Unrestricted_Access);
      Arg2 := ConvertLoc (Locator'Unrestricted_Access);
      Ret_Code_To_Exception
        (DDS_DataWriter_Get_Matched_Subscription_Datawriter_Protocol_Status_By_Locator
           (Self.GetInterface, Arg1, Arg2));
   end Get_Matched_Subscription_Datawriter_Protocol_Status_By_Locator;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (Self    : not null access Ref)
   is
   begin
      Ret_Code_To_Exception (DDS_DataWriter_Flush (Self.GetInterface));
   end Flush;

   -----------
   -- Write --
   -----------

   procedure WriteI
     (Self          : not null access Ref;
      Instance_Data : in System.Address;
      Handle        : in DDS.InstanceHandle_T_Access;
      MetpImpl      : DDS.MetpTypeSupport.Unsupported'Class)
   is
   begin
      if MetpImpl.Metp_Supported and Self.Is_Metp_Writer (MetpImpl) then
         declare
            Retcode  : DDS.ReturnCode_T;
            W_Params : aliased DDS.WriteParams_T;
         begin
            Initialize (W_Params);
            Retcode := MetpImpl.DataWriter_Write_Metp_Data
              (Self.GetInterface, Instance_Data, Handle, W_Params'Unchecked_Access);
            Finalize (W_Params);
            Ret_Code_To_Exception (Retcode, "unable to write METP data");
         end;
      else
         Ret_Code_To_Exception (DDS_DataWriter_Write_Untyped_GeneralI (Self.GetInterface,
                                null,
                                null,
                                Instance_Data,
                                DDS_InstanceHandle_T (Handle.all)'Unrestricted_Access),
                                "unable to write message");
      end if;
   end WriteI;

   function CreateI (Publisher    : DDS.Publisher.Ref_Access;
                     Topic        : access DDS.Topic.Ref'Class;
                     Qos          : in DDS.DataWriterQos;
                     Listener     : in DDS.DataWriterListener.Ref_Access;
                     Mask         : in DDS.StatusMask)
                     return DDS.DataWriter.Ref_Access is
      type TempT is access all PRESWord;

      function Convert is new Ada.Unchecked_Conversion (TempT, DDS.TypeSupport.Ref_Access);

      Need_To_Enable        : aliased RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Common_H.DDS_Boolean := 0;
      TopicI                : constant DDS.Topic_Impl.Ref_Access := DDS.Topic_Impl.Ref_Access (Topic);
      C_Writer_Ptr          : System.Address;
      C_DataTypeUtility_Ptr : access RTIDDS.Low_Level.Ndds_Pres_Pres_TypePlugin_H.PRESTypePlugin;
      TypeSupport_Access    : DDS.TypeSupport.Ref_Access;
      Typed_Writer          : DDS.DataWriter.Ref_Access;
      Typed_Writer_Impl     : Ref_Access;
      Typed_Writer_Impl_A   : Ref_Access_Access;
      Created               : Boolean := False;
      Qos_Access            : access DDS_DataWriterQos;
      use type DDS.DataWriterListener.Ref_Access;
      Type_Name             : Interfaces.C.Strings.chars_ptr;
   begin
      --  Check if default QoS must be used
      if Qos'Address = DDS.Publisher.DATAWRITER_QOS_DEFAULT'Address then
         Qos_Access := DDS_DATAWRITER_QOS_DEFAULT'Unrestricted_Access;
      elsif Qos'Address = DDS.Publisher.DATAWRITER_QOS_USE_TOPIC_QOS'Address then
         Qos_Access := DDS_DATAWRITER_QOS_USE_TOPIC_QOS'Unrestricted_Access;
      else
         Qos_Access := GetInterface (Qos);
      end if;

      --  Create Disabled C Data Writer
      C_Writer_Ptr := DDS_Publisher_Create_Datawriter_DisabledI
        (Self        => Publisher.GetInterface,
         Need_Enable => Need_To_Enable'Unchecked_Access,
         Topic       => TopicI.As_TopicWrapper_I,
         Qos         => Qos_Access,
         Listener    => null, --  will re-set after Ada object created and before enable
         Mask        => DDS_StatusMask (Mask));

      if C_Writer_Ptr /= System.Null_Address then
         Type_Name := DDS_TopicDescription_Get_Type_Name
           (DDS_DataWriter_Get_Topic (C_Writer_Ptr).U_As_TopicDescription);

         C_DataTypeUtility_Ptr := DDS_DomainParticipant_Get_Type_PluginI
           (DDS_Publisher_Get_Participant (Publisher.GetInterface),
            Type_Name);

         if C_DataTypeUtility_Ptr = null then
            raise ERROR with "Unable to Create DataWriter, user type not registered with participant";
         end if;

         TypeSupport_Access := Convert (C_DataTypeUtility_Ptr.U_UserBuffer.all'Access);

         if TypeSupport_Access = null then
            raise ERROR with "Unable to Create DataWriter, user type not registered";
         end if;

         Typed_Writer := TypeSupport_Access.Create_TypedDataWriterI;
         Typed_Writer_Impl := DDS.DataWriter_Impl.Ref_Access (Typed_Writer);

         Typed_Writer_Impl.Entity_Initialize_I (C_Writer_Ptr);

         Typed_Writer_Impl_A :=  Convert (DDS_Entity_Get_User_DataI (C_Writer_Ptr));

         if Typed_Writer_Impl_A /= null then
            Typed_Writer_Impl_A.all := Typed_Writer_Impl;

            --  Set up Listener
            if Listener /= null then
               Typed_Writer_Impl.Set_Listener (Listener, Mask);
            end if;

            if Need_To_Enable /= 0 then
               Typed_Writer_Impl.Enable;
            end if;
            Created := True;
         end if;
      end if;

      if Created  then
         return Typed_Writer;
      else
         RTIDDS.Obj_Impl.Free (RTIDDS.Obj_Impl.Ref_Access (Typed_Writer));
         raise ERROR with "Unable to Create DataWriter";
      end if;

   end CreateI;

   function  CreateI (Publisher    : DDS.Publisher.Ref_Access;
                      Topic        : access DDS.Topic.Ref'Class;
                      Library_Name : in DDS.String;
                      Profile_Name : in DDS.String;
                      Listener     : in DDS.DataWriterListener.Ref_Access;
                      Mask         : in DDS.StatusMask)
                      return DDS.DataWriter.Ref_Access is
      QOS     : aliased DDS.DataWriterQos;
      Factory : constant DDS.DomainParticipantFactory.Ref_Access :=
                  DDS.DomainParticipantFactory.Get_Instance;
   begin
      Publisher.Get_Default_DataWriter_Qos (QOS);
      Factory.Get_Datawriter_Qos_From_Profile_W_Topic_Name
        (QOS, Library_Name, Profile_Name, Topic.Get_Name);
      return CreateI (Publisher, Topic, Qos, Listener, Mask);
   end CreateI;


   function Get_FacadeI (C_DataWriter : System.Address)
                         return Ref_Access is
      P_Impl        : Ref_Access := null;
      P_Impl_Access : Ref_Access_Access;
   begin

      if C_DataWriter /= System.Null_Address then
         P_Impl_Access :=
           Convert (DDS_Entity_Get_User_DataI (C_DataWriter));
         if P_Impl_Access /= null then
            P_Impl := P_Impl_Access.all;
         end if;
      end if;

      return P_Impl;

   end Get_FacadeI;

   function Register_Instance
     (Self          : not null access Ref;
      Instance_Data : System.Address)
      return DDS.InstanceHandle_T
   is
      Ret : RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
   begin
      Ret := DDS_DataWriter_Register_Instance_UntypedI (Self.GetInterface,
                                                        Instance_Data);
      return InstanceHandle_T (Ret);
   end Register_Instance;


   function Register_Instance_W_Timestamp
     (Self             : not null access Ref;
      Instance_Data    : System.Address;
      Source_Timestamp : in DDS.Time_T)
      return DDS.InstanceHandle_T
   is
      Ret         : RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
      Timestamp   : aliased DDS_Time_T;
   begin
      Copy_Down (Timestamp, Source_Timestamp);
      Ret := DDS_DataWriter_Register_Instance_W_Timestamp_UntypedI (Self.GetInterface,
                                                                    Instance_Data,
                                                                    Timestamp'Unrestricted_Access);
      return InstanceHandle_T (Ret);
   end Register_Instance_W_Timestamp;

   function Register_Instance_W_Params
     (Self             : not null access Ref;
      Instance_Data    : System.Address;
      Params           : access DDS.WriteParams_T)
      return Standard.DDS.InstanceHandle_T is
      Ret         : RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
      type DDS_WriteParams_T_Access is access
        all RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_WriteParams_T with
          Storage_Size => 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => DDS_WriteParams_T_Access);
   begin

      Ret := DDS_DataWriter_Register_Instance_W_Params_UntypedI
        (Self     => Self.GetInterface,
         Instance => Instance_Data,
         Params   => Convert (Params.all'Address));
      return InstanceHandle_T (Ret);
   end Register_Instance_W_Params;


   procedure Unregister_Instance
     (Self          : not null access Ref;
      Instance_Data : System.Address;
      Handle        : in DDS.InstanceHandle_T;
      MetpImpl      : DDS.MetpTypeSupport.Unsupported'Class)
   is
      H : aliased RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
   begin
      Copy_Down (H, Handle);
      Ret_Code_To_Exception (DDS_DataWriter_Unregister_Instance_Untyped_GeneralI
                             (Self.GetInterface,
                                null,
                                Instance_Data,
                                H'Access));
      if MetpImpl.Metp_Supported and then Instance_Data /= System.Null_Address and then Self.Is_Metp_Writer (MetpImpl) then
         Self.Delete_Metp_Data (Instance_Data, MetpImpl);
      end if;
   end Unregister_Instance;

   procedure Unregister_Instance_W_Timestamp
     (Self              : not null access Ref;
      Instance_Data     : System.Address;
      Handle            : in DDS.InstanceHandle_T;
      Source_Timestamp  : in DDS.Time_T;
      MetpImpl          : DDS.MetpTypeSupport.Unsupported'Class)
   is
      H           : aliased RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
      Timestamp   : aliased DDS_Time_T;
   begin
      Copy_Down (Timestamp, Source_Timestamp);
      Copy_Down (H, Handle);
      Ret_Code_To_Exception (DDS_DataWriter_Unregister_Instance_W_Timestamp_Untyped_GeneralI
                             (Self.GetInterface,
                                null,
                                Instance_Data,
                                H'Access,
                                Timestamp'Access));
      if MetpImpl.Metp_Supported and then Instance_Data /= System.Null_Address and then Self.Is_Metp_Writer (MetpImpl) then
         Self.Delete_Metp_Data (Instance_Data, MetpImpl);
      end if;
   end Unregister_Instance_W_Timestamp;

   procedure Unregister_Instance_W_Params
     (Self              : not null access Ref;
      Instance_Data     : System.Address;
      Params            : access DDS.WriteParams_T;
      MetpImpl          : DDS.MetpTypeSupport.Unsupported'Class)
   is
      type DDS_WriteParams_T_Access is access
        all RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_WriteParams_T with
          Storage_Size => 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => DDS_WriteParams_T_Access);
   begin
      Ret_Code_To_Exception
        (DDS_DataWriter_Unregister_Instance_W_Params_Untyped_GeneralI
           (Self                 => Self.GetInterface,
            Remote_Reader_Handle => null,
            Instance             => Instance_Data,
            Params               => Convert (Params.all'Address)));
      if MetpImpl.Metp_Supported and then Instance_Data /= System.Null_Address and then Self.Is_Metp_Writer (MetpImpl) then
         Self.Delete_Metp_Data (Instance_Data, MetpImpl);
      end if;
   end Unregister_Instance_W_Params;



   procedure Write_W_Timestamp
     (Self             : not null access Ref;
      Instance_Data    : System.Address;
      Handle           : in DDS.InstanceHandle_T_Access;
      Source_Timestamp : in DDS.Time_T;
      MetpImpl         : DDS.MetpTypeSupport.Unsupported'Class)
   is
      H           : aliased RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
      Timestamp   : aliased DDS_Time_T;
   begin
      if MetpImpl.Metp_Supported and then Self.Is_Metp_Writer (MetpImpl) then
         declare
            Retcode  : DDS.ReturnCode_T;
            W_Params : aliased DDS.WriteParams_T;
         begin
            Initialize (W_Params);
            W_Params.Source_Timestamp := Source_Timestamp;
            Retcode := MetpImpl.DataWriter_Write_Metp_Data (Self.GetInterface, Instance_Data, Handle, W_Params'Unchecked_Access);
            Finalize (W_Params);
            Ret_Code_To_Exception (Retcode, "unable to write METP data");
         end;
      else
         Copy_Down (Timestamp, Source_Timestamp);
         Copy_Down (H, Handle.all);
         Ret_Code_To_Exception (DDS_DataWriter_Write_W_Timestamp_Untyped_GeneralI
                                (Self.GetInterface,
                                   null,
                                   null,
                                   Instance_Data,
                                   H'Access,
                                   Timestamp'Access));
      end if;
   end Write_W_Timestamp;

   procedure Write_W_Params
     (Self             : not null access Ref;
      Instance_Data    : System.Address;
      Params           : in out DDS.WriteParams_T;
      MetpImpl         : DDS.MetpTypeSupport.Unsupported'Class)
   is
      type DDS_WriteParams_T_Access is access
        all RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_WriteParams_T with
          Storage_Size => 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => DDS.WriteParams_T_Access,
         Target => DDS_WriteParams_T_Access);
   begin
      if MetpImpl.Metp_Supported and Self.Is_Metp_Writer (MetpImpl) then
         if Params.Cookie /= COOKIE_DEFAULT then
            raise ILLEGAL_OPERATION with "cookie set in write parameters";
         end if;
         declare
            Retcode : DDS.ReturnCode_T;
         begin
            Retcode := MetpImpl.DataWriter_Write_Metp_Data
              (Self.GetInterface,
               Instance_Data,
               Params.Handle'Unrestricted_Access,
               Params'Unrestricted_Access);
            Finalize (Params.Cookie);
            Ret_Code_To_Exception (Retcode, "unable to write METP data");
         end;
      else
         Ret_Code_To_Exception (DDS_DataWriter_Write_W_Params_Untyped_GeneralI
                                (Self                 => Self.GetInterface,
                                 Remote_Reader_Handle => null,
                                 Data                 => Instance_Data,
                                 Params               => Convert (Params'Unrestricted_Access)));
      end if;

   end Write_W_Params;


   procedure Dispose
     (Self             : not null access Ref;
      Instance_Data    : System.Address;
      Instance_Handle  : in DDS.InstanceHandle_T;
      MetpImpl         : DDS.MetpTypeSupport.Unsupported'Class)
   is
      H : aliased RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
   begin
      Copy_Down (H, Instance_Handle);
      Ret_Code_To_Exception (DDS_DataWriter_Dispose_Untyped_GeneralI
                             (Self.GetInterface,
                                null,
                                null,
                                Instance_Data,
                                H'Access));

      if MetpImpl.Metp_Supported and then Instance_Data /= System.Null_Address and then Self.Is_Metp_Writer (MetpImpl) then
         Self.Delete_Metp_Data (Instance_Data, MetpImpl);
      end if;
   end Dispose;

   procedure Dispose_W_Timestamp
     (Self              : not null access Ref;
      Instance_Data     : System.Address;
      Instance_Handle   : in DDS.InstanceHandle_T;
      Source_Timestamp  : in DDS.Time_T;
      MetpImpl          : DDS.MetpTypeSupport.Unsupported'Class)
   is
      H           : aliased RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
      Timestamp   : aliased DDS_Time_T;
   begin
      Copy_Down (Timestamp, Source_Timestamp);
      Copy_Down (H, Instance_Handle);
      Ret_Code_To_Exception (DDS_DataWriter_Dispose_W_Timestamp_Untyped_GeneralI
                             (Self.GetInterface,
                                null,
                                null,
                                Instance_Data,
                                H'Access,
                                Timestamp'Access));

      if MetpImpl.Metp_Supported and then Instance_Data /= System.Null_Address and then Self.Is_Metp_Writer (MetpImpl) then
         Self.Delete_Metp_Data (Instance_Data, MetpImpl);
      end if;
   end Dispose_W_Timestamp;

   procedure Dispose_W_Params
     (Self              : not null access Ref;
      Instance_Data     : System.Address;
      Params            : in DDS.WriteParams_T;
      MetpImpl          : DDS.MetpTypeSupport.Unsupported'Class)
   is
      type DDS_WriteParams_T_Access is access
        all RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Infrastructure_H.DDS_WriteParams_T with
          Storage_Size => 0;

      function Convert is new Ada.Unchecked_Conversion
        (Source => DDS.WriteParams_T_Access,
         Target => DDS_WriteParams_T_Access);
   begin
      Ret_Code_To_Exception (DDS_DataWriter_Dispose_W_Params_Untyped_GeneralI
                             (Self                 => Self.GetInterface,
                              Remote_Reader_Handle => null,
                              Data                 => Instance_Data,
                              Params               => Convert (Params'Unrestricted_Access)));

      if MetpImpl.Metp_Supported and then Instance_Data /= System.Null_Address and then Self.Is_Metp_Writer (MetpImpl) then
         Self.Delete_Metp_Data (Instance_Data, MetpImpl);
      end if;
   end Dispose_W_Params;



   procedure Get_Key_Value
     (Self        : not null access Ref;
      Key_Holder  : System.Address;
      Handle      : in DDS.InstanceHandle_T)
   is
      H          : aliased RTIDDS.Low_Level.Ndds_Pres_Pres_Participant_H.PRESInstanceHandle;
   begin
      Copy_Down (H, Handle);
      Ret_Code_To_Exception (DDS_DataWriter_Get_Key_Value_UntypedI
                             (Self.GetInterface,
                                Key_Holder,
                                H'Access));
   end Get_Key_Value;

   function Lookup_Instance
     (Self        : not null access Ref;
      Key_Holder  : System.Address) return DDS.InstanceHandle_T
   is
      Ret   : DDS.InstanceHandle_T;
   begin
      Copy_Up (Ret,
               DDS_DataWriter_Lookup_Instance_UntypedI (Self.GetInterface,
                 Key_Holder));
      return Ret;
   end Lookup_Instance;

   procedure Free (This : in out Ref_Access)
   is
      P_Impl_Access : Ref_Access_Access;
   begin
      if This = null then
         return;
      end if;

      P_Impl_Access := Convert (DDS_Entity_Get_User_DataI (This.GetInterface));

      Free_Mem (This);
      This := null;

      if P_Impl_Access /= null then
         P_Impl_Access.all := null;
      end if;
   end Free;

   procedure Finalize_Callback
     (Arg1 : System.Address;
      Arg2 : System.Address;
      Arg3 : access RTIDDS.Low_Level.Ndds_Reda_Reda_Worker_Impl_H.REDAWorker)
   is
      pragma Unreferenced (Arg1);
      pragma Unreferenced (Arg3);
      P_Impl_Access : Ref_Access_Access;
      P_Impl        : Ref_Access;
   begin
      if Arg2 = System.Null_Address then
         return;
      end if;

      P_Impl_Access := Convert (Arg2);

      if P_Impl_Access = null then
         return;
      end if;

      P_Impl := P_Impl_Access.all;

      if P_Impl = null then
         return;
      end if;

      Free (P_Impl);
   end Finalize_Callback;


   function Is_Metp_Writer
     (Self     : not null access Ref;
      MetpImpl : DDS.MetpTypeSupport.Unsupported'Class) return Standard.Boolean is
   begin
      return MetpImpl.DataWriter_Is_Metp_Writer (Self.GetInterface);
   end Is_Metp_Writer;

   procedure Delete_Metp_Data
     (Self          : not null access Ref;
      Instance_Data : System.Address;
      MetpImpl      : DDS.MetpTypeSupport.Unsupported'Class)
   is
      Ret : DDS.ReturnCode_T;
      pragma Warnings (Off, Ret);
   begin
      Ret := MetpImpl.DataWriter_Delete_Metp_Data (Self.GetInterface, Instance_Data);
      --  Ignore return value as done in the C code.
   end Delete_Metp_Data;


   procedure Get_LoanI
     (Self            : not null access Ref;
      Loan_Ret        : out System.Address;
      Initialize_Data : out Standard.Boolean;
      MetpImpl        : DDS.MetpTypeSupport.Unsupported'Class) is
   begin
      if not Self.Is_Metp_Writer (MetpImpl) then
         raise UNSUPPORTED with
           "get_loan only supported for topic-types with transfer mode shmem-ref";
      end if;

      Ret_Code_To_Exception (MetpImpl.DataWriter_Create_Metp_Data
                             (Self.GetInterface, Loan_Ret, Initialize_Data));

   end Get_LoanI;


   procedure Discard_LoanI
     (Self     : not null access Ref;
      Sample   : System.Address;
      MetpImpl : DDS.MetpTypeSupport.Unsupported'Class) is
   begin
      if not Self.Is_Metp_Writer (MetpImpl) then
         raise UNSUPPORTED with
           "discard_loan only supported for topic-types with transfer mode shmem-ref";
      end if;

      Ret_Code_To_Exception (MetpImpl.DataWriter_Delete_Metp_Data (Self.GetInterface, Sample));

   end Discard_LoanI;


end DDS.DataWriter_Impl;
