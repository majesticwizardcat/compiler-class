import unittest
from compiler import (Argument, FunctionEntity, ParameterEntity, SymbolTable,
                      VariableEntity)


class SymbolTableTest(unittest.TestCase):
    def test_parameter_passing_on_new_scope(self):
        tbl = SymbolTable()
        print('scopes on init', tbl.scopes)
        tbl.create_scope()
        tbl.add_entity(VariableEntity("foo"))
        tbl.add_entity(FunctionEntity("fn", 42))
        tbl.add_argument(Argument("bat", "cv"))
        tbl.add_argument(Argument("man", "ref"))
        tbl.create_scope()

        print(tbl.scopes[-1].entities)
        self.assertEqual(tbl.scopes[-1].entities, [
            ParameterEntity(name="bat", mode="cv", offset=12),
            ParameterEntity(name="man", mode="ref", offset=16)
        ])

    def test_parameter_passing_on_deeper_scopes(self):
        tbl = SymbolTable()
        print('scopes on init', tbl.scopes)
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

        print(tbl.scopes[-1].entities)
        self.assertEqual(tbl.scopes[-1].entities, [
            ParameterEntity(name="x", mode="cv", offset=12),
            ParameterEntity(name="y", mode="ref", offset=16)
        ])


if __name__ == '__main__':
    unittest.main()
