import unittest
from compiler import (Argument, FunctionEntity, ParameterEntity, Scope,
                      SymbolTable, TempVariableEntity, VariableEntity)


class SymbolTableTest(unittest.TestCase):
    def test_parameter_passing_on_new_scope(self):
        tbl = SymbolTable()
        tbl.create_scope()
        tbl.add_entity(VariableEntity("foo"))
        tbl.add_entity(FunctionEntity("fn", 42))
        tbl.add_argument(Argument("bat", "cv"))
        tbl.add_argument(Argument("man", "ref"))
        tbl.create_scope()

        self.assertEqual(tbl.scopes[-1].entities, [
            ParameterEntity(name="bat", mode="cv", offset=12),
            ParameterEntity(name="man", mode="ref", offset=16)
        ])

    def test_parameter_passing_on_deeper_scopes(self):
        tbl = SymbolTable()
        tbl.create_scope()
        tbl.add_entity(VariableEntity("foo"))
        tbl.add_entity(FunctionEntity("fn", 42))
        tbl.add_argument(Argument("bat", "cv"))
        tbl.add_argument(Argument("man", "ref"))
        tbl.create_scope()
        tbl.add_entity(VariableEntity("baz"))
        tbl.add_entity(FunctionEntity("fn2", 84))
        tbl.add_argument(Argument("x", "cv"))
        tbl.add_argument(Argument("y", "ref"))
        tbl.create_scope()

        self.assertEqual(tbl.scopes[-1].entities, [
            ParameterEntity(name="x", mode="cv", offset=12),
            ParameterEntity(name="y", mode="ref", offset=16)
        ])

    def test_exam_example(self):
        tbl = SymbolTable()
        tbl.create_scope()
        tbl.add_entity(VariableEntity("a"))
        tbl.add_entity(VariableEntity("b"))
        tbl.add_entity(FunctionEntity("P1", 3))
        tbl.add_argument(Argument("x", "cv"))
        tbl.add_argument(Argument("y", "ref"))

        tbl.create_scope()
        tbl.add_entity(VariableEntity("c"))
        tbl.add_entity(VariableEntity("d"))
        tbl.add_entity(FunctionEntity("P11", 6))
        tbl.add_argument(Argument("w", "cv"))
        tbl.add_argument(Argument("z", "ref"))

        tbl.create_scope()
        tbl.add_entity(VariableEntity("e"))
        tbl.add_entity(FunctionEntity("P21", 8))
        tbl.add_argument(Argument("x", "cv"))

        tbl.create_scope()
        tbl.add_entity(TempVariableEntity("T_0"))

        self.assertEqual(len(tbl.scopes), 4)
        self.assertEqual(
            tbl.scopes[0],
            Scope(
                entities=[
                    VariableEntity(name="a", offset=12),
                    VariableEntity(name="b", offset=16),
                    FunctionEntity(
                        name="P1",
                        start_quad=3,
                        arguments=[Argument("x", "cv"),
                                   Argument("y", "ref")])
                ],
                nesting_level=0))

        self.assertEqual(
            tbl.scopes[1],
            Scope(
                entities=[
                    ParameterEntity(name="x", mode="cv", offset=12),
                    ParameterEntity(name="y", mode="ref", offset=16),
                    VariableEntity(name="c", offset=20),
                    VariableEntity(name="d", offset=24),
                    FunctionEntity(
                        name="P11",
                        start_quad=6,
                        arguments=[Argument("w", "cv"),
                                   Argument("z", "ref")])
                ],
                nesting_level=1))

        self.assertEqual(
            tbl.scopes[2],
            Scope(
                entities=[
                    ParameterEntity(name="w", mode="cv", offset=12),
                    ParameterEntity(name="z", mode="ref", offset=16),
                    VariableEntity(name="e", offset=20),
                    FunctionEntity(
                        name="P21",
                        start_quad=8,
                        arguments=[Argument("x", "cv")])
                    # TODO: add framelength (20) above ^
                ],
                nesting_level=2))

        self.assertEqual(tbl.scopes[3],
                         Scope(
                             entities=[
                                 ParameterEntity(
                                     name="x", mode="cv", offset=12),
                                 TempVariableEntity(name="T_0", offset=16)
                             ],
                             nesting_level=3))

        tbl.destroy_scope()
        self.assertEqual(tbl.scopes[-1],
                         Scope(
                             entities=[
                                 ParameterEntity(
                                     name="w", mode="cv", offset=12),
                                 ParameterEntity(
                                     name="z", mode="ref", offset=16),
                                 VariableEntity(name="e", offset=20),
                                 FunctionEntity(
                                     name="P21",
                                     start_quad=8,
                                     arguments=[Argument("x", "cv")],
                                     frame_length=20)
                             ],
                             nesting_level=2))

    def test_framelength(self):
        tbl = SymbolTable()
        tbl.create_scope()
        tbl.add_entity(VariableEntity("foo"))
        tbl.add_entity(FunctionEntity("fn", 42))
        tbl.add_argument(Argument("bat", "cv"))
        tbl.add_argument(Argument("man", "ref"))
        tbl.create_scope()
        tbl.add_entity(VariableEntity("baz"))
        tbl.destroy_scope()

        self.assertEqual(tbl.scopes[-1].entities, [
            VariableEntity(name="foo", offset=12),
            FunctionEntity(
                name="fn",
                start_quad=42,
                arguments=[Argument("bat", "cv"),
                           Argument("man", "ref")],
                frame_length=24)
        ])

    def test_no_framelength_set_on_no_scopes(self):
        tbl = SymbolTable()
        tbl.create_scope()
        tbl.add_entity(VariableEntity("foo"))
        tbl.destroy_scope()


if __name__ == '__main__':
    unittest.main()
