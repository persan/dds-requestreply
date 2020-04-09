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
with Ada.Unchecked_Conversion;
with Interfaces.C.Pointers;
with Interfaces.C;
with DDS.Treats_Generic;
--  <defgroup>OctetsBuiltInTypeGroupDocs</defgroup>
--  <dref>OctetsTypeSupport</dref>
package DDS.Builtin_Octets_TypeSupport is

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
   --  <dref>OctetsTypeSupport_register_type</dref>

   procedure Unregister_Type
     (Participant : not null access Standard.DDS.DomainParticipant.Ref'Class;
      Type_Name   : in Standard.DDS.String);
   --  <dref>OctetsTypeSupport_unregister_type</dref>

   function Get_Type_Name return Standard.DDS.String;
   --  <dref>OctetsTypeSupport_get_type_name</dref>

   procedure Print_Data (A_Data : access DDS.Octets);
   --  <dref>OctetsTypeSupport_print_data</dref>

   function Create_Data_W_Size
     (Size : Integer)
      return DDS.Octets_Ptr;

   procedure Initialize_Data_W_Size
     (A_Data : access DDS.Octets;
      Size   : Integer);

   function From_Octets_To_Octet_Array
     (Octets : DDS.Octets)
      return DDS.Octet_Array;

   function Get_Octet
     (Octets : DDS.Octets;
      Index  : Integer)
      return Interfaces.Unsigned_8;

   --  <internal>
   --  Methods to translate from low to high level and backward
   --  </internal>

   package Octet_Pointer is new Interfaces.C.Pointers
     (Natural,
      DDS.Octet,
      DDS.Octet_Array,
      0);

   type Octet_Access is access all Interfaces.Unsigned_8;
   function ConvertToAccess is new Ada.Unchecked_Conversion (System.Address, Octet_Access);
   function ConvertToPointer is new Ada.Unchecked_Conversion (Octet_Access, Octet_Pointer.Pointer);
   function ConvertToPointerDiff  (Source : Integer) return Interfaces.C.Ptrdiff_T;
   pragma Inline (ConvertToPointerDiff);


   package Treats is new
     DDS.Treats_Generic (Data_Type        => DDS.Octets,
                         Data_Type_Access => DDS.Octets_Ptr,
                         Index_Type       => Natural,
                         First_Element    => 1,
                         Data_Array       => DDS.Octets_Array,
                         Initialize       => Initialize,
                         Finalize         => Finalize,
                         Copy             => Copy,
                         Data_Sequences   => DDS.Octets_Seq,
                         Get_Type_Name    => Get_Type_Name,
                         TypeSupport      => Ref,
                         MetpTypeSupport  => DDS.MetpTypeSupport.Unsupported);
   procedure Get_Type_Name (Name : out Standard.DDS.String);


end DDS.Builtin_Octets_TypeSupport;
