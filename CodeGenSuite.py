import unittest
from TestUtils import TestCodeGen
from AST import *


class CheckCodeGenSuite(unittest.TestCase):
    # Test build-in function and literal
    def test_print_int(self):
        """Simple program: int main() {} """
        input = """void main() {
                           putInt(7);
                       }"""
        expect = "7"
        self.assertTrue(TestCodeGen.test(input, expect, 500))

    def test_test(self):
        """Simple program: int main() {} """
        input = """
        void main() {
            int a,b;
            b =1;
            if( true){
                b =2;
            }
            putInt(b);
        }"""
        expect = ""
        self.assertTrue(TestCodeGen.test(input, expect, 600))

#     def test_print_float(self):
#         """Simple program: int main() {} """
#         input = """void main() {
#                         putFloat(7.7);
#                     }"""
#         expect = "7.7"
#         self.assertTrue(TestCodeGen.test(input, expect, 501))
#
#     def test_print_string(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putString("Hello world");
#                 }"""
#         expect = "Hello world"
#         self.assertTrue(TestCodeGen.test(input, expect, 502))
#
#     def test_print_boolean(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putBool(true);
#                 }"""
#         expect = "true"
#         self.assertTrue(TestCodeGen.test(input, expect, 503))
#
#     def test_putIntLn_function(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putIntLn(1);
#                 }"""
#         expect = "1\n"
#         self.assertTrue(TestCodeGen.test(input, expect, 504))
#
#     def test_putFloatLn_function(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putFloatLn(10.0);
#                 }"""
#         expect = "10.0\n"
#         self.assertTrue(TestCodeGen.test(input, expect, 505))
#
#     def test_putBoolLn_function(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putBoolLn(true);
#                 }"""
#         expect = "true\n"
#         self.assertTrue(TestCodeGen.test(input, expect, 506))
#
#     def test_putStringLn_function(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putStringLn("Hello world!");
#                 }"""
#         expect = "Hello world!\n"
#         self.assertTrue(TestCodeGen.test(input, expect, 507))
#
#     def test_putLn(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putBool(true);
#                 putLn();
#                 putInt(1);
#                 }"""
#         expect = "true\n1"
#         self.assertTrue(TestCodeGen.test(input, expect, 508))
#
#     # Test simple expression not include assign
#     def test_simple_add_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putIntLn(1+2);
#                 putFloatLn(1.5+2.5);
#                 putFloatLn(1+2.5);
#                 }"""
#         expect = """3
# 4.0
# 3.5
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 509))
#
#     def test_simple_minus_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putIntLn(1-2);
#                 putFloatLn(2.5-1.2);
#                 putFloatLn(10.5-5);
#                 }"""
#         expect = """-1
# 1.3
# 5.5
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 510))
#
#     def test_simple_multiply_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putIntLn(2*3);
#                 putFloatLn(2.5*1.2);
#                 putFloatLn(1.5* 10);
#                 }"""
#         expect = """6
# 3.0
# 15.0
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 511))
#
#     def test_simple_divide_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putIntLn(3/2);
#                 putFloatLn(3.6/1.2);
#                 putFloatLn(6.4/4);
#                 }"""
#         expect = """1
# 2.9999998
# 1.6
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 512))
#
#     def test_simple_module_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putIntLn(3%2);
#                 putIntLn(6%10);
#                 putIntLn(100%33);
#                 }"""
#         expect = """1
# 6
# 1
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 513))
#
#     def test_simple_larger_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putBoolLn(1 > 2);
#                 putBoolLn(3 > 1);
#                 putBoolLn(2 > 1 +1);
#                 }"""
#         expect = """false
# true
# false
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 514))
#
#     def test_simple_larger_than_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                putBoolLn(1 >= 2);
#                 putBoolLn(3 >= 1);
#                 putBoolLn(2 >= 1 +1);
#                 }"""
#         expect = """false
# true
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 515))
#
#     def test_simple_smaller_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putBoolLn(99 < 100);
#                 putBoolLn(31 < 12);
#                 putBoolLn(4 < 3 +1);
#                 }"""
#         expect = """true
# false
# false
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 516))
#
#     def test_simple_smaller_than_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putBoolLn(99 <= 100);
#                 putBoolLn(31 <= 12);
#                 putBoolLn(4 <= 3 +1);
#                 }"""
#         expect = """true
# false
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 517))
#
#     def test_simple_and_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putBoolLn(true && false);
#                 putBoolLn(false && true && true);
#                 putBoolLn(false && (false && true));
#                 }"""
#         expect = """false
# false
# false
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 518))
#
#     def test_simple_or_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putBoolLn(true || false);
#                 putBoolLn(false || true || true);
#                 putBoolLn(false || (false || true));
#                 }"""
#         expect = """true
# true
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 519))
#
#     def test_simple_equal_expression(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                     putBoolLn(true == false);
#                     putBoolLn(1 == 1);
#                     putBoolLn( 1+1 == 2);
#                     }"""
#         expect = """false
# true
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 520))
#
#     def test_simple_not_equal_expression(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                     putBoolLn(true != false);
#                     putBoolLn(false != true || true);
#                     putBoolLn(1 != 1 );
#                     }"""
#         expect = """true
# true
# false
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 521))
#
#     def test_print_simple_negative_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putIntLn(-1);
#                 putFloatLn(-1.5);
#                 putFloatLn(-(10+2.5));
#                 }"""
#         expect = """-1
# -1.5
# -12.5
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 522))
#
#     def test_print_simple_not_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 putBoolLn(!true);
#                 putBoolLn(!false);
#                 putBoolLn(!(1==1));
#                 }"""
#         expect = """false
# true
# false
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 523))
#
#     def test_simple_assign_global_variable(self):
#         """Simple program: int main() {} """
#         input = """
#             int a;
#             void main() {
#                 a =1;
#                 putInt(a);
#                 }"""
#         expect = "1"
#         self.assertTrue(TestCodeGen.test(input, expect, 524))
#
#     def test_simple_assign_local__variable(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 int a;
#                 a = 1;
#                 putInt(a);
#                 }"""
#         expect = "1"
#         self.assertTrue(TestCodeGen.test(input, expect, 525))
#
#     def test_simple_assign_multiple_variables(self):
#         """Simple program: int main() {} """
#         input = """
#             int a;
#             boolean b;
#             string c;
#             float d;
#             void main() {
#                 a =1;
#                 b = true;
#                 c = "Hello world";
#                 d = 1.2;
#                 putIntLn(a);
#                 putBoolLn(b);
#                 putStringLn(c);
#                 putFloatLn(d);
#                 }"""
#         expect = """1
# true
# Hello world
# 1.2
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 526))
#
#     def test_simple_assign_multiple_variables_in_1_line(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = b= c = 1;
#                 putIntLn(a);
#                 putIntLn(b);
#                 putIntLn(c);
#                 }"""
#         expect = """1
# 1
# 1
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 527))
#
#     def test_complex_add_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = b= c = 1;
#                 putIntLn(a +10 +(b + 20));
#                 putIntLn(b + (a +(a - b)));
#                 putIntLn((c+ (a + b)) + c);
#                 }"""
#         expect = """32
# 2
# 4
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 528))
#
#     def test_complex_minus_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = b= c = 69;
#                 putIntLn(a - b - (c-10));
#                 putFloatLn(b - (a -( c- a - 100.1)));
#                 putFloatLn((a - 2.5)- (c - 212.1) - b);
#                 }"""
#         expect = """-59
# -100.100006
# 140.6
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 529))
#
#     def test_complex_multiple_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a;
#             float b,c;
#             void main() {
#                 a = 1;
#                 b= c = 1.5;
#                 putIntLn(a* (10 * (1 +3)));
#                 putFloatLn((b* 1.5) * (20*40));
#                 putFloatLn((a * 2.5) * (c - 2.1) * b);
#                 }"""
#         expect = """40
# 1800.0
# -2.2499995
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 530))
#
#     def test_complex_divide_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a;
#             float b,c;
#             void main() {
#                 a = 1;
#                 b= c = 1.5;
#                 putIntLn(100 / (10 / (1 +3)));
#                 putFloatLn((b* 1.5) / (20*40));
#                 putFloatLn((a / 2.5) /((c / 2.1) / b));
#                 }"""
#         expect = """50
# 0.0028125
# 0.84000003
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 531))
#
#     def test_complex_module_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 2;
#                 b = 99;
#                 c = 10;
#                 putIntLn(a%(b%20));
#                 putIntLn(b%c%a);
#                 putIntLn(c%(b%(a%10)));
#                 }"""
#         expect = """2
# 1
# 0
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 532))
#
#     def test_complex_equal_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 4;
#                 b = 68;
#                 c = 100;
#                 putBoolLn( (a==4) == (b == 68));
#                 putBoolLn(b == (100 + (a + 64)));
#                 putBoolLn(a + b == b - c );
#                 }"""
#         expect = """true
# false
# false
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 533))
#
#     def test_complex_not_equal_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 4;
#                 b = 68;
#                 c = 100;
#                 putBoolLn( (a != 4) != (b != 68));
#                 putBoolLn(b != (100 - (a + 64)));
#                 putBoolLn(a + b != b - c );
#                 }"""
#         expect = """false
# true
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 534))
#
#     def test_complex_greater_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 4;
#                 b = 68;
#                 c = 100;
#                 putBoolLn( (a - 204) > (b * 30));
#                 putBoolLn( ( b - a+c) > (68%100 * 10));
#                 putBoolLn( (b > c) == (a > 100) );
#                 }"""
#         expect = """false
# false
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 535))
#
#     def test_complex_greater_than_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 4;
#                 b = 68;
#                 c = 100;
#                 putBoolLn( (b + 100) >= (c * 2));
#                 putBoolLn( ( b + a + c) >= ( c % 50 * 2));
#                 putBoolLn( (a >= c) == (b >= 100) );
#                 }"""
#         expect = """false
# true
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 536))
#
#     def test_complex_smaller_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 4;
#                 b = 68;
#                 c = 100;
#                 putBoolLn( (a + 699) < (b * 11));
#                 putBoolLn( ( b*5 + a*4 + c) < ( c % 150 * 19));
#                 putBoolLn( (a < c) != (b < 100) );
#                 }"""
#         expect = """true
# true
# false
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 537))
#
#     def test_complex_smaller_than_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 4;
#                 b = 68;
#                 c = 100;
#                 putBoolLn( (a + 699) <= (b * 11));
#                 putBoolLn( ( 18*5 + a + c) <= ( b % 150 * 19));
#                 putBoolLn( (100 <= c) != (c <= 100) );
#                 }"""
#         expect = """true
# true
# false
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 538))
#
#     def test_complex_and_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 4;
#                 b = 68;
#                 c = 100;
#                 putBoolLn( (a==4) && (b == 68));
#                 putBoolLn((b == c -a) && (100 == (a + 64)));
#                 putBoolLn((a >= b) && (b > c) );
#                 }"""
#         expect = """true
# false
# false
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 539))
#
#     def test_complex_or_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 4;
#                 b = 68;
#                 c = 100;
#                 putBoolLn( (a== c -b) || (c == a * 16));
#                 putBoolLn(c == (b + (a + 64)));
#                 putBoolLn((a == b +100) || (b*3 != c) );
#                 }"""
#         expect = """false
# false
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 540))
#
#     def test_complex_assign_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 putIntLn((a = 4) + 1);
#                 putIntLn(b = (100 + a + 64));
#                 putIntLn(c = b *a );
#                 }"""
#         expect = """5
# 168
# 672
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 541))
#
#     def test_mix_binary_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#             a = 4;
#             b = 680;
#             c = 999;
#                 putIntLn((a = 4) + 1 - 20 /8 %10);
#                 putBoolLn((b * 3 -c) == (100 + a + 64));
#                 putFloatLn(1.5 -a*4.5 + 10 /c - b%6);
#                 }"""
#         expect = """3
# false
# -18.5
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 542))
#
#     def test_complex_negative_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 3;
#                 b =199;
#                 c = 1212;
#                 putIntLn(-((a = 4) + 1));
#                 putIntLn(-(100 + a  + 64));
#                 putIntLn(-(c / b *a%209) );
#                 }"""
#         expect = """-5
# -168
# -24
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 543))
#
#     def test_complex_not_expression(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 a = 12;
#                 b = 11314;
#                 c = 21231;
#                 putBoolLn(!((c -b + a) <  10000));
#                 putBoolLn(!(b *10 - (100 + a + 64) == c));
#                 putBoolLn(!(c + a > b*2) );
#                 }"""
#         expect = """false
# true
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 544))
#
#     def test_assign_local_variable_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                     int a;
#                     a = 0;
#                     putIntLn(a);
#                     a =2;
#                     putIntLn(a);
#                     }"""
#         expect = """0
# 2
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 545))
#
#     def test_multiple_local_variables_assign_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                     int a,b,c;
#                     a = b = c = 0;
#                     a = a = a+1;
#                     putIntLn(a);
#                     putIntLn(b);
#                     putIntLn(c);
#                     }"""
#         expect = """1
# 0
# 0
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 546))
#
#     def test_multiple_local_variables_assign_statement_different_type(self):
#         """Simple program: int main() {} """
#         input = """
#                     void main() {
#                         int a;
#                         float b;
#                         string c;
#                         boolean d;
#                         a =10;
#                         b =6.9;
#                         c = "Hello world";
#                         d = true;
#                         putIntLn(a);
#                         putFloatLn(b);
#                         putStringLn(c);
#                         putBoolLn(d);
#                         }"""
#         expect = """10
# 6.9
# Hello world
# true
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 547))
#
#     # statement
#     def test_simple_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#             int a;
#             void main() {
#                 a = 0;
#                 {
#                     a = 10;
#                     putIntLn(a);
#                 }
#                 putIntLn(a);
#                 }"""
#         expect = """10
# 10
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 548))
#
#     def test_initial_variable_inside_block(self):
#         """Simple program: int main() {} """
#         input = """
#             int a;
#             void main() {
#                 a = 0;
#                 {
#                     int a;
#                     a = 2;
#                     putIntLn(a);
#                 }
#                 putIntLn(a);
#             }"""
#         expect = """2
# 0
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 549))
#
#     def test_initial_variable_in_2_blocks(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                a = 0;
#                {
#                     int a;
#                     a = 6;
#                     putIntLn(a);
#                 }
#                 {
#                     float a;
#                     a = 1;
#                     putFloatLn(a);
#                 }
#             }"""
#         expect = """6
# 1.0
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 550))
#
#     def test_block_statement_inside_block_statement_with_global_variable(self):
#         """Simple program: int main() {} """
#         input = """
#             int a;
#             void main() {
#                 a = 0;
#                 {
#                     a = 2;
#                     putIntLn(a);
#                     {
#                         a =3;
#                         putIntLn(a);
#                     }
#                 }
#                 putIntLn(a);
#             }"""
#         expect = """2
# 3
# 3
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 551))
#
#     def test_block_statement_inside_block_statement_with_local_variable(self):
#         """Simple program: int main() {} """
#         input = """
#             void main() {
#                 int a;
#                 a = 0;
#                 {
#                     a = 2;
#                     putIntLn(a);
#                     {
#                         a =3;
#                         putIntLn(a);
#                     }
#                 }
#                 putIntLn(a);
#             }"""
#         expect = """2
# 3
# 3
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 552))
#
#     def test_complex_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#             int a,b,c;
#             void main() {
#                 int a;
#                   a = 0;
#                   {
#                       a = 2;
#                       putIntLn(a);
#                       {
#                           a =3;
#                           {
#                            a =4;
#                           }
#                           putIntLn(a);
#                       }
#
#                       {
#                         a =1;
#                       }
#                   }
#                   putIntLn(a);
#             }"""
#         expect = """2
# 4
# 1
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 553))
#
#     def test_simple_if_no_else_statement_with_literal_exp(self):
#         """Simple program: int main() {} """
#         input = """
#             int x;
#             void main() {
#                 x = 0;
#                 if(true)
#                     putIntLn(x);
#
#                 if(false)
#                     putIntLn(x+1);
#
#                 putIntLn(x+2);
#             }"""
#         expect = """0
# 2
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 554))
#
#     def test_simple_if_have_else_statement_with_literal_exp(self):
#         """Simple program: int main() {} """
#         input = """
#             int x;
#             void main() {
#                  x = 0;
#                 if(false)
#                     putIntLn(x);
#                 else
#                     putIntLn(x+1);
#
#                 putIntLn(x+2);
#                 }"""
#         expect = """1
# 2
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 555))
#
#     def test_if_no_else_with_complex_binary_exp(self):
#         """Simple program: int main() {} """
#         input = """
#             int x;
#             void main() {
#                  x = 0;
#                 if(x == 0)
#                     putIntLn(x);
#
#                 if(x!= 1)
#                     putIntLn(x+1);
#                 putIntLn(x+2);
#                 }"""
#         expect = """0
# 1
# 2
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 556))
#
#     def test_if_have_else_with_complex_binary_exp(self):
#         """Simple program: int main() {} """
#         input = """
#              int x;
#             void main() {
#                 x = 0;
#                 if(x == 0)
#                     putIntLn(x);
#                 else
#                     putIntLn(x+1);
#
#                 if(x != 0)
#                     putIntLn(x);
#                 else
#                     putIntLn(x+1);
#
#                 putIntLn(x+2);
#                 }"""
#         expect = """0
# 1
# 2
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 557))
#
#     def test_if_no_else_statement_with_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#             int x;
#             void main() {
#                 x = 0;
#                 if(x == 0){
#                     x =10;
#                     putIntLn(x);
#                 }
#
#                 if(x != 0){
#                     x =0;
#                     putIntLn(x);
#                 }
#
#                 putIntLn(x+2);
#                 }"""
#         expect = """10
# 0
# 2
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 558))
#
#     def test_if_have_else_statement_with_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#             int x;
#             void main() {
#                 x = 0;
#                 if(x == 0){
#                     x = 10;
#                 }
#                 else{
#                     x = 12;
#                 }
#
#                 if(x == 0){
#                     x =10;
#                 }
#                 else{
#                     x =12;
#                 }
#
#                 putIntLn(x+2);
#                 }"""
#         expect = """14
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 559))
#
#     def test_complex_if_no_else_statement(self):
#         """Simple program: int main() {} """
#         input = """
#             int x;
#             void main() {
#                 x = 0;
#                 if(x == 0){
#                     x =10;
#                     if(x != 10){
#                         x = 12;
#                     }
#                     x =69;
#                     putIntLn(x);
#                 }
#                 putIntLn(x);
#                 }"""
#         expect = """69
# 69
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 560))
#
#     def test_complex_if_have_else_statement(self):
#         """Simple program: int main() {} """
#         input = """
#             int x;
#             void main() {
#                 x = 0;
#                 if(x == 0){
#                     x =10;
#                     putIntLn(x);
#                     if(x != 0)
#                         x =0;
#                     else
#                         putIntLn(x);
#                 }
#                 else{
#                     if(x <10)
#                         x =10;
#                     else
#                         putIntLn(x);
#                 }
#                 putIntLn(x);
#                 }"""
#         expect = """10
# 0
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 561))
#
#     def test_simple_do_while_with_literal_exp(self):
#         """Simple program: int main() {} """
#         input = """
#             int x;
#             void main() {
#                 x = 0;
#                 do
#                     x = 1;
#                 while(false);
#                 putIntLn(x);
#                 }"""
#         expect = """1
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 562))
#
#     def test_simple_do_while_with_binary_op(self):
#         """Simple program: int main() {} """
#         input = """
#             int x;
#             void main() {
#                 x = 1;
#                 do
#                     x = x * 100;
#                 while(x == 1);
#                 putIntLn(x);
#                 }"""
#         expect = """100
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 563))
#
#     def test_do_while_with_list_of_expr_statement(self):
#         """Simple program: int main() {} """
#         input = """
#            int x;
#             void main() {
#                 x = 1;
#                 do
#                     putIntLn(x);
#                     x = x+1;
#                 while(x < 4);
#                 putIntLn(x);
#                 }"""
#         expect = """1
# 2
# 3
# 4
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 564))
#
#     def test_do_while_with_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#             int y;
#             void main() {
#                 y = 2;
#                 do{
#                     putIntLn(y);
#                     y = y*2;
#                 }
#                 while(y < 16);
#                 putIntLn(y);
#                 }"""
#         expect = """2
# 4
# 8
# 16
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 565))
#
#     def test_do_while_with_list_of_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#             int y;
#             void main() {
#                 y = 2;
#                 do{
#                     putIntLn(y);
#                     y = y*2;
#                 }
#                 {
#                     y = y*2;
#                 }
#                 while(y < 16);
#                 putIntLn(y);
#                 }"""
#         expect = """2
# 8
# 32
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 566))
#
#     def test_do_while_with_if_statement(self):
#         """Simple program: int main() {} """
#         input = """
#             int z;
#             void main() {
#                 z = 0;
#                 do
#                     if(z < 3)
#                         z = z+1;
#                     putIntLn(z);
#                     z = z +1;
#                 while(z < 5);
#                 putIntLn(z);
#                 }"""
#         expect = """1
# 3
# 4
# 5
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 567))
#
#     def test_complex_do_while_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 int z;
#                 void main() {
#                     z = 0;
#                     do{
#                         if(z < 10)
#                             z = z+3;
#                         putIntLn(z);
#                         z = z +3;
#                     }
#                     {
#                         if( z >5)
#                             z = z+ 3;
#                         putIntLn(z);
#                     }
#                     while(z < 15);
#                     putIntLn(z);
#                     }"""
#         expect = """3
# 9
# 12
# 18
# 18
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 568))
#
#     def test_simple_for_statement_with_literal_expression(self):
#         """Simple program: int main() {} """
#         input = """
#                 int i;
#                 void main() {
#                     for( i =1; false; i = i+1)
#                         putIntLn(i);
#                     putIntLn(i);
#                 }"""
#         expect = """1
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 569))
#
#     def test_for_loop_with_binary_boolean_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1)
#                         putIntLn(i);
#                     putIntLn(i);
#                 }"""
#         expect = """1
# 2
# 3
# 4
# 5
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 570))
#
#     def test_for_loop_with_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1){
#                         i = i*2;
#                         putIntLn(i);
#                     }
#                     putIntLn(i);
#                 }"""
#         expect = """2
# 6
# 7
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 571))
#
#     def test_for_loop_with_if_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1)
#                     if(i <3){
#                         i = i*2;
#                         putIntLn(i);
#                     }
#                     putIntLn(i);
#                 }"""
#         expect = """2
# 5
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 572))
#
#     def test_for_loop_with_do_while_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1)
#                     do{
#                         putIntLn(i);
#                         i = i*2;
#                     }
#                     while( i< 3);
#                     putIntLn(i);
#                 }"""
#         expect = """1
# 2
# 5
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 573))
#
#     def test_complex_for_loop(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                 for( i =1; i < 10; i = i+1){
#                     if(i > 5){
#                         i = i+1;
#                         putIntLn(i);
#                     }
#                     else{
#                         do
#                             i = i+1;
#                         while(i < 5);
#                     }
#                 }
#                     putIntLn(i);
#                 }"""
#         expect = """7
# 9
# 10
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 574))
#
#     def test_simple_break_statement_inside_for_loop(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1)
#                         break;
#                     putIntLn(i);
#                 }"""
#         expect = """1
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 575))
#
#     def test_break_statement_inside_block_inside_for_loop(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1){
#                         {
#                             break;
#                         }
#                     }
#                     putIntLn(i);
#                 }"""
#         expect = """1
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 576))
#
#     def test_break_statement_inside_if_inside_for_loop(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1){
#                         if(i  == 3)
#                             break;
#                         putIntLn(i);
#                     }
#                     putIntLn(i);
#                 }"""
#         expect = """1
# 2
# 3
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 577))
#
#     def test_simple_break_inside_do_while_loop(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int a;
#                 a =1;
#                 do
#                     a = a + 1;
#                     break;
#                 while(a <5);
#                 putIntLn(a);
#                 }"""
#         expect = """2
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 578))
#
#     def test_break_inside_block_inside_do_while(self):
#         """Simple program: int main() {} """
#         input = """
#                  void main() {
#                 int b;
#                 b =1;
#                 do{
#                     b = b * 100;
#                     break;
#                 }
#                 while(b <5);
#                 putIntLn(b);
#                 }"""
#         expect = """100
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 579))

    def test_break_inside_if_inside_do_while(self):
        """Simple program: int main() {} """
        input = """
            void main() {
                int b;
                b =1;
                do{
                    b = b * 100;
                    break;
                }
                while(b <5);
                putIntLn(b);
            }"""
        expect = """2
6
7
"""
        self.assertTrue(TestCodeGen.test(input, expect, 580))
#
#     def test_for_loop_with_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1){
#                         i = i*2;
#                         putIntLn(i);
#                     }
#                     putIntLn(i);
#                 }"""
#         expect = """2
# 6
# 7
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 571))
#
#     def test_for_loop_with_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1){
#                         i = i*2;
#                         putIntLn(i);
#                     }
#                     putIntLn(i);
#                 }"""
#         expect = """2
# 6
# 7
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 571))
#
#
#     def test_for_loop_with_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1){
#                         i = i*2;
#                         putIntLn(i);
#                     }
#                     putIntLn(i);
#                 }"""
#         expect = """2
# 6
# 7
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 571))
#
#     def test_for_loop_with_block_statement(self):
#         """Simple program: int main() {} """
#         input = """
#                 void main() {
#                 int i;
#                   for( i =1; i < 5; i = i+1){
#                         i = i*2;
#                         putIntLn(i);
#                     }
#                     putIntLn(i);
#                 }"""
#         expect = """2
# 6
# 7
# """
#         self.assertTrue(TestCodeGen.test(input, expect, 571))

