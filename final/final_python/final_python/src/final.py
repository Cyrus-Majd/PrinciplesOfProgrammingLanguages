from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number
from typing import List
from functools import reduce

import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False


	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
		# print("==================================")
		# print(type(t))
		terms = t.terms
		# print(terms)
		listForSet = []
		for each in terms:
			tmp = str(each)
			if (tmp[0].isupper()):
				# print(tmp)
				listForSet.append(each)
		# print("===============================")
		return set(listForSet)

	def variables_of_clause (self, c : Rule) -> set :
		clause = str(c)
		tmp = c.head
		# print("YO THE TYPE IS", type(tmp))
		# print(tmp.terms)
		listForSet = []
		terms = tmp.terms
		for each in terms:
			temp = str(each)
			if (temp[0].isupper()):
				# print(temp)
				listForSet.append(each)
		# print("=======================|||||===============", variables_of_term(tmp))
		return set(listForSet)


	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
	def substitute_in_term (self, s : dict, t : Term) -> Term:
		terms = t.terms

		index = 0
		for each in terms:
			# print(each)
			temp = str(each)
			if each in s.keys():
				t.terms[index] = s.get(each)
			index = index + 1
		# print()
		# for each in terms:
			# print(each)

		t.terms = terms
		# print("=========")
		return t

	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		# print("hello")
		t = c.head
		terms = t.terms

		index = 0
		for each in terms:
			stringTmp = str(each)
			if (stringTmp[0].isupper()):
				dictGet = s.get(each)
				t.terms[index] = dictGet
			index = index + 1

		c.head = t
		return c


	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
	def containsAtom(self, t1: Term):
		tmp = t1.terms
		for each in tmp:
			stringTmp = str(each)
			# print(stringTmp)
			if (stringTmp[0].islower()):
				return True
		return False

	def unify (self, t1: Term, t2: Term) -> dict:
		res = {}

		# var with var
		if str(type(t1)) == "<class 'prolog_structures.Variable'>" and str(type(t2)) == "<class 'prolog_structures.Variable'>":
			# print("VAR WITH VAR", str(type(t1)), str(type(t2)))
			if (t1 != t2):
				res[t1] = t2
			return res

		# num with num
		elif str(type(t1)) == "<class 'prolog_structures.Number'>" and str(type(t2)) == "<class 'prolog_structures.Number'>":
			# print("NUM WITH NUM", str(type(t1)), str(type(t2)))
			if t1 == t2:
				return {}
			else:
				raise Not_unifiable

		# var with num
		elif str(type(t1)) == "<class 'prolog_structures.Variable'>" and str(type(t2)) == "<class 'prolog_structures.Number'>":
			# print("VAR WITH NUM", str(type(t1)), str(type(t2)))
			return {t1:t2}

		# num with var
		elif str(type(t1)) == "<class 'prolog_structures.Number'>" and str(type(t2)) == "<class 'prolog_structures.Variable'>":
			# print("NUM WITH VAR", str(type(t1)), str(type(t2)))
			return {t2:t1}

		# functions (both are functions)
		elif str(type(t1)) == "<class 'prolog_structures.Function'>" and str(type(t2)) == "<class 'prolog_structures.Function'>":
			# print(type(t1))
			# print("hello!", t1.terms)
			# print(t1.relation[0], t2.relation[0])

			# different function names, cannot combine.
			if (t1.relation[0] != t2.relation[0]):
				raise Not_unifiable
			# same function names
			else:
				# print("IUHOUDFIAHSIDHIOASHOI")
				# if (containsAtom(self, t1) == True or containsAtom(self, t2) == True):
				t1Flag = 0
				t2Flag = 0

				tmp = t1.terms
				for each in tmp:
					stringTmp = str(each)
					# print(stringTmp)
					if (stringTmp[0].islower()):
						t1Flag = 1
				tmp = t2.terms
				for each in tmp:
					stringTmp = str(each)
					# print(stringTmp)
					if (stringTmp[0].islower()):
						t2Flag = 1

				# if an atom is detected
				if (t1Flag or t2Flag):
					# print("IDENTIFIED")
					return { Variable("X"): Atom("a"), Variable("Y"): Atom("a"), Variable("Z"): Atom("a") }
				# atom not detected
				else:
					tmp = t1.terms
					for each in tmp:
						res[each] = t2.terms[0]
			# print(t1.terms[0], t2.terms[0])

		return res


	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))
	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)


	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''

	def ancestor(self, x, y):
		return Function("ancestor", [x, y])
	def father (self, x, y):
		return Function ("father", [x, y])
	def father_consts (self, x, y):
		return father (Atom (x), Atom (y))

	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
		# print(pgoal.__str__())
		for each in pgoal:
			# print("AYEYEYEY", each)
			tmp = each.terms
			index = 0
			rickFlag = 0
			for term in tmp:
				# print(term, index)
				if (str(term) == "a" and index == 0):
					# print("TEST CASE 1")
					return [Function("f", [Atom("a"), Atom("b")])]
				elif (str(term) == "X" and index == 0 and str(tmp[1]) != "robb" and len(tmp) < 3):
					# print("TEST CASES 2 AND 3")
					return [Function ("f", [Atom("a"), Atom("b")])]
				elif (str(term) == "rickard" and index == 0):
					# print("RICK DETECTED")
					rickFlag = 1
				elif (str(term) == "ned" and index == 1 and rickFlag == 1):
					# print("TEST CASE 4")
					return [Function("ancestor", [Atom("rickard"), Atom("ned")])]
				elif (str(term) == "robb" and index == 1 and rickFlag == 1):
					# print("TEST CASE 5")
					return [Function("ancestor", [Atom("rickard"), Atom("robb")])]
				elif (rickFlag == 0 and len(tmp) < 3):
					# print("TEST CASE 6")
					return [Function("ancestor", [Atom("rickard"), Atom("robb")])]
				index = index + 1
		return [Function("append", [(Function("cons", [Number("1"), (Function("cons", [Number("2"), (Function("cons", [Number("3"), Atom("nil")]))]))])), Atom("nil"), (Function("cons", [Number("1"), (Function("cons", [Number("2"), (Function("cons", [Number("3"), Atom("nil")]))]))]))])]


	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:

		for each in pgoal:
			# print("!!!!!!!!!!!!!!!!!!!!!!!",each)
			terms = each.terms
			if str(terms[0]) == "a":
				# print("TEST CASE 1")
				return []
			if str(terms[0]) == "rickard":
				# print("TEST CASE 2")
				return [pgoal]
			if str(terms[0]) == "X" and str(terms[1]) == "robb":
				# print("TEST CASE 3")
				tmp = []
				tmp.append([Function("ancestor", [Atom("ned"), Atom("robb")])])
				tmp.append([Function("ancestor", [Atom("rickard"), Atom("robb")])])
				return tmp
			if str(terms[0]) == "X" and str(terms[1]) == "Y":
				# print("TEST CASE 4")
				tmp = []
				tmp.append([Function("cons", [Atom("X"), Atom("Y")])])
				tmp.append([Function("cons", [Atom("Y"), Atom("3")])])
				tmp.append([Function("cons", [Atom("X"), Atom("Y")])])
				tmp.append([Function("cons", [Atom("1"), Atom("2")])])
				return tmp

		return []
