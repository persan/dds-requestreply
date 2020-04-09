--  (c) Copyright, Real-Time Innovations, $Date:: 2012-02-16 #$
--  All rights reserved.
--
--  No duplications, whole or partial, manual or electronic, may be made
--  without express written permission.  Any such copies, or
--  revisions thereof, must display this notice unaltered.
--  This code contains trade secrets of Real-Time Innovations, Inc.

with DDS;
with DDS.DomainParticipant;
with DDS.TypeSupport;
with DDS.MetpTypeSupport;
with DDS.DataReader;
with DDS.DataWriter;
with DDS.Treats_Generic;

--  <defgroup>KeyedStringBuiltInTypeGroupDocs</defgroup>
--  <dref>KeyedStringTypeSupport</dref>
package DDS.Builtin_KeyedString_TypeSupport is

   type Ref is new Standard.DDS.TypeSupport.Ref with null record;
   type Ref_Access is access all Ref'Class;

   function Create_TypedDataReaderI
     (Self : access Ref) return Standard.DDS.DataReader.Ref_Access;

   procedure Destroy_TypedDataReaderI
     (Self   : access Ref;
      Reader : in out Standard.DDS.DataReader.Ref_Access);

   function Create_TypedDataWriterI
     (Self : access Ref) return Standard.DDS.DataWriter.Ref_Access;

   procedure Destroy_TypedDataWriterI
     (Self   : access Ref;
      Writer : in out Standard.DDS.DataWriter.Ref_Access) is null;

   --  <internal>
   --  static methods
   --  </internal>

   procedure Register_Type
     (Participant : not null access Standard.DDS.DomainParticipant.Ref'Class;
      Type_Name   : in Standard.DDS.String);
   --  <dref>KeyedStringTypeSupport_register_type</dref>

   procedure Unregister_Type
     (Participant : not null access Standard.DDS.DomainParticipant.Ref'Class;
      Type_Name   : in Standard.DDS.String);
   --  <dref>KeyedStringTypeSupport_unregister_type</dref>

   function Get_Type_Name return Standard.DDS.String;
   --  <dref>KeyedStringTypeSupport_get_type_name</dref>


   function Create_Data (AllocatePointers : in Boolean := True)
                         return DDS.KeyedString;

   procedure Delete_Data
     (A_Data : in out DDS.KeyedString; DeletePointers : in Boolean := True);

   procedure Print_Data (A_Data : access DDS.KeyedString);
   --  <dref>KeyedStringTypeSupport_print_data</dref>

   function Create_Data_W_Size
     (Key_Size : Integer;
      Size     : Integer)
      return DDS.KeyedString;

   procedure Initialize_Data_W_Size
     (A_Data   : access DDS.KeyedString;
      Key_Size : Integer;
      Size     : Integer);


   package Treats is new
     DDS.Treats_Generic (Data_Type        => DDS.KeyedString,
                         Data_Type_Access => DDS.KeyedString_Ptr,
                         Index_Type       => Natural,
                         First_Element    => 1,
                         Data_Array       => DDS.KeyedString_Array,
                         Initialize       => Initialize,
                         Finalize         => Finalize,
                         Copy             => Copy,
                         Data_Sequences   => DDS.KeyedString_Seq,
                         Get_Type_Name    => Get_Type_Name,
                         TypeSupport      => Ref,
                         MetpTypeSupport  => DDS.MetpTypeSupport.Unsupported);
   procedure Get_Type_Name
     (Name : out Standard.DDS.String);
end DDS.Builtin_KeyedString_TypeSupport;
