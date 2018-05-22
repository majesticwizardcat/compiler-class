import unittest
from compiler import (Argument, FinalGen, FunctionEntity, LookupResult,
                      ParameterEntity, Scope, SymbolTable, TempVariableEntity,
                      VariableEntity)
from unittest.mock import MagicMock


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

        tbl.fill_in_framelength_on_callee()
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
        tbl.fill_in_framelength_on_callee()
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
        tbl.fill_in_framelength_on_callee()
        tbl.destroy_scope()

    def test_add_entity_on_empty_nested_scope_gives_correct_offset(self):
        tbl = SymbolTable()
        tbl.create_scope()
        tbl.add_entity(VariableEntity("varfoo1"))
        tbl.add_entity(
            FunctionEntity("procfoo", start_quad=42, type="procedure"))
        tbl.create_scope()
        tbl.add_entity(VariableEntity("varfoo2"))

        last = tbl.last_entity()
        self.assertEqual(len(tbl.scopes[-1].entities), 1)
        self.assertEqual(last.name, "varfoo2")
        self.assertEqual(last.offset, 12)


class FinalCodeHelpersTest(unittest.TestCase):
    def test_gnlvcode_nesting_level_1(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=1)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.gnlvcode('var0'), ['lw $t0, -4($sp)', 'add $t0, $t0, -4'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('var0')

    def test_gnlvcode_nesting_level_2(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=2)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.gnlvcode('var0'),
            ['lw $t0, -4($sp)', 'lw $t0, -4($t0)', 'add $t0, $t0, -4'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('var0')

    def test_gnlvcode_nesting_level_3(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=3)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.gnlvcode('var0'), [
                'lw $t0, -4($sp)', 'lw $t0, -4($t0)', 'lw $t0, -4($t0)',
                'add $t0, $t0, -4'
            ])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('var0')

    def test_loadvr_constant(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock()
        tbl.lookup = MagicMock()

        gen = FinalGen(tbl)

        self.assertEqual(gen.loadvr('42', 0), ['li $t0, 42'])
        tbl.get_current_nesting_level.assert_not_called()
        tbl.lookup.assert_not_called()

    def test_loadvr_global_var(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=0)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('var0', 4), 0))

        gen = FinalGen(tbl)

        self.assertEqual(gen.loadvr('var0', 0), ['lw $t0, -4($s0)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('var0')

    def test_loadvr_same_nesting_level_local(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=1)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('local_var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(gen.loadvr('local_var0', 0), ['lw $t0, -4($sp)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('local_var0')

    def test_loadvr_same_nesting_level_in_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=1)
        tbl.lookup = MagicMock(
            return_value=LookupResult(ParameterEntity('in_par0', 'cv', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(gen.loadvr('in_par0', 0), ['lw $t0, -4($sp)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('in_par0')

    def test_loadvr_same_nesting_level_temp(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=1)
        tbl.lookup = MagicMock(
            return_value=LookupResult(TempVariableEntity('temp_var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(gen.loadvr('temp_var0', 0), ['lw $t0, -4($sp)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('temp_var0')

    def test_loadvr_same_nesting_level_inout_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=1)
        tbl.lookup = MagicMock(
            return_value=LookupResult(
                ParameterEntity('inout_par0', 'ref', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.loadvr('inout_par0', 0), ['lw $t0, -4($sp)', 'lw $t0, ($t0)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('inout_par0')

    def test_loadvr_one_diff_nesting_level_local(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=2)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('local_var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.loadvr('local_var0', 1),
            gen.gnlvcode('local_var0') + ['lw $t1, ($t0)'])

    def test_loadvr_one_diff_nesting_level_in_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=2)
        tbl.lookup = MagicMock(
            return_value=LookupResult(ParameterEntity('in_par0', 'cv', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.loadvr('in_par0', 1),
            gen.gnlvcode('in_par0') + ['lw $t1, ($t0)'])

    def test_loadvr_one_diff_nesting_level_inout_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=2)
        tbl.lookup = MagicMock(
            return_value=LookupResult(
                ParameterEntity('inout_par0', 'ref', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.loadvr('inout_par0', 1),
            gen.gnlvcode('inout_par0') + ['lw $t0, ($t0)', 'lw $t1, ($t0)'])

    def test_loadvr_smaller_nesting_level_local(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=3)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('local_var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.loadvr('local_var0', 1),
            gen.gnlvcode('local_var0') + ['lw $t1, ($t0)'])

    def test_loadvr_smaller_nesting_level_in_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=3)
        tbl.lookup = MagicMock(
            return_value=LookupResult(ParameterEntity('in_par0', 'cv', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.loadvr('in_par0', 1),
            gen.gnlvcode('in_par0') + ['lw $t1, ($t0)'])

    def test_loadvr_smaller_nesting_level_inout_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=3)
        tbl.lookup = MagicMock(
            return_value=LookupResult(
                ParameterEntity('inout_par0', 'ref', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.loadvr('inout_par0', 1),
            gen.gnlvcode('inout_par0') + ['lw $t0, ($t0)', 'lw $t1, ($t0)'])

    def test_storerv_global_var(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=0)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('global_var0', 4), 0))

        gen = FinalGen(tbl)

        self.assertEqual(gen.storerv(2, 'global_var0'), ['sw $t2, -4($s0)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('global_var0')

    def test_storerv_same_nesting_level_local(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=1)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('local_var0', 8), 1))

        gen = FinalGen(tbl)

        self.assertEqual(gen.storerv(3, 'local_var0'), ['sw $t3, -8($sp)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('local_var0')

    def test_storerv_same_nesting_level_in_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=1)
        tbl.lookup = MagicMock(
            return_value=LookupResult(ParameterEntity('in_par0', 'cv', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(gen.storerv(3, 'in_par0'), ['sw $t3, -4($sp)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('in_par0')

    def test_storerv_same_nesting_level_temp(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=1)
        tbl.lookup = MagicMock(
            return_value=LookupResult(TempVariableEntity('temp_var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(gen.storerv(4, 'temp_var0'), ['sw $t4, -4($sp)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('temp_var0')

    def test_storerv_same_nesting_level_inout_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=1)
        tbl.lookup = MagicMock(
            return_value=LookupResult(
                ParameterEntity('inout_par0', 'ref', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.storerv(1, 'inout_par0'), ['lw $t0, -4($sp)', 'sw $t1, ($t0)'])
        tbl.get_current_nesting_level.assert_called_once()
        tbl.lookup.assert_called_once_with('inout_par0')

    def test_storerv_one_diff_nesting_level_local(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=2)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('local_var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.storerv(1, 'local_var0'),
            gen.gnlvcode('local_var0') + ['sw $t1, ($t0)'])

    def test_storerv_one_diff_nesting_level_in_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=2)
        tbl.lookup = MagicMock(
            return_value=LookupResult(ParameterEntity('in_par0', 'cv', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.storerv(5, 'in_par0'),
            gen.gnlvcode('in_par0') + ['sw $t5, ($t0)'])

    def test_storerv_one_diff_nesting_level_inout_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=2)
        tbl.lookup = MagicMock(
            return_value=LookupResult(
                ParameterEntity('inout_par0', 'ref', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.storerv(3, 'inout_par0'),
            gen.gnlvcode('inout_par0') + ['lw $t0, ($t0)', 'sw $t3, ($t0)'])

    def test_storerv_smaller_nesting_level_local(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=3)
        tbl.lookup = MagicMock(
            return_value=LookupResult(VariableEntity('local_var0', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.storerv(1, 'local_var0'),
            gen.gnlvcode('local_var0') + ['sw $t1, ($t0)'])

    def test_storerv_smaller_nesting_level_in_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=3)
        tbl.lookup = MagicMock(
            return_value=LookupResult(ParameterEntity('in_par0', 'cv', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.storerv(3, 'in_par0'),
            gen.gnlvcode('in_par0') + ['sw $t3, ($t0)'])

    def test_storerv_smaller_nesting_level_inout_parameter(self):
        tbl = SymbolTable()
        tbl.get_current_nesting_level = MagicMock(return_value=3)
        tbl.lookup = MagicMock(
            return_value=LookupResult(
                ParameterEntity('inout_par0', 'ref', 4), 1))

        gen = FinalGen(tbl)

        self.assertEqual(
            gen.storerv(5, 'inout_par0'),
            gen.gnlvcode('inout_par0') + ['lw $t0, ($t0)', 'sw $t5, ($t0)'])


if __name__ == '__main__':
    unittest.main()
