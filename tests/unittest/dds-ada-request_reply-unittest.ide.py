import GPS
import os
from os.path import *
import gs_utils
import libadalang as lal

TEMPLATE_REGISISTER_TESTS = """separate (%(parent)s)
--  begin read only
procedure Register_Tests (Test : in out Test_Case) is
   use AUnit.Test_Cases.Registration;
begin
%(registers)s
end Register_Tests;
--  end read only
"""

TEMPLATE_REGISISTER =\
    """   Register_Routine (Test, %(name)s'Access, "%(name)s");"""


TEMPLATE_TESTCASE_SPEC = """
with AUnit.Test_Cases;
with AUnit;
package %(name)s is

   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      null;
   end record;

   function Name (Test : Test_Case) return AUnit.Message_String;

   procedure Register_Tests (Test : in out Test_Case);
   --  Register test methods with test suite

   procedure Set_Up_Case (Test : in out Test_Case);
   procedure Tear_Down_Case (Test : in out Test_Case);

end %(name)s;
"""

TEMPLATE_TEST_ROUTINE=\
"""

   procedure %(name)s (Test : in out Aunit.Test_Cases.Test_Case'Class) is
      Tc : Test_Case renames Test_Case(Test);
   begin
      null;
   end %(name)s;"""

TEMPLATE_TESTCASE_BODY = \
"""with GNAT.Source_Info;
with AUnit; use AUnit;
package body %(name)s is

   ----------
   -- Name --
   ----------
   Test_Name : constant Standard.String := GNAT.Source_Info.Enclosing_Entity;
   function Name (Test : Test_Case) return AUnit.Message_String is
   begin
      return Format (Test_Name);
   end Name;

%(testroutines)s

   procedure Register_Tests (Test : in out Test_Case) is separate;
   -----------------
   -- Set_Up_Case --
   -----------------

   procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      null;
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   procedure Tear_Down_Case (Test : in out Test_Case) is
   begin
      null;
   end Tear_Down_Case;

end %(name)s;"""


def find_routines(path):
    ret = []
    context = lal.AnalysisContext()
    unit = context.get_from_file(path)
    if unit.root:
        for node in unit.root.finditer(lambda n: n.is_a(lal.PackageBody)):
            unit_name = node.children[0].children[0].text

        for node in unit.root.finditer(lambda n: n.is_a(lal.SubpSpec)):
            i = node.children[1]
            name = i.text
            for e in node.children[2].finditer(lambda n: n.is_a(lal.SubtypeIndication)):
                if e.text == "Aunit.Test_Cases.Test_Case'Class":
                    ret.append(name)
    return unit_name, ret


def ada2file(name):
    return name.lower().replace(".", "-")


@gs_utils.hook("file_saved")
def on_file_saved(f):
    register_test = splitext(f.name())[0]+"-register_tests.adb"
    if exists(register_test):
        registers = []
        parent, routines = find_routines(f.name())
        for r in routines:
            registers.append(TEMPLATE_REGISISTER % {"name": r})
        with open(register_test, "w") as outf:
            outf.write(TEMPLATE_REGISISTER_TESTS %
                       {"parent": parent,
                        "registers": "\n".join(registers)})


on_file_saved(GPS.File("dds-request_reply-requester-impl-unittest.adb"))
