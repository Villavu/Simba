#!/bin/env python

import sys
import re
import string
import base64

template = base64.b64decode(b'dW5pdCBscCRDbGFzc05hbWU7Ci8vRGVwZW5kczogJERlcGVuZHMKCnskbW9kZSBvYmpmcGN9eyRIK30KeyRJIFNpbWJhLmluY30KCmludGVyZmFjZQoKdXNlcwogIENsYXNzZXMsIFN5c1V0aWxzLCBscGNvbXBpbGVyLCBscHR5cGVzLCBscENsYXNzSGVscGVyOwoKcHJvY2VkdXJlIFJlZ2lzdGVyXyRDbGFzc05hbWUoQ29tcGlsZXI6IFRMYXBlQ29tcGlsZXIpOwoKaW1wbGVtZW50YXRpb24KCnR5cGUKICAkQ2xhc3NOYW1lUCA9IF4kQ2xhc3NOYW1lOwoKJE1ldGhvZHMKCnByb2NlZHVyZSBSZWdpc3Rlcl8kQ2xhc3NOYW1lKENvbXBpbGVyOiBUTGFwZUNvbXBpbGVyKTsKYmVnaW4KICB3aXRoIENvbXBpbGVyIGRvCiAgYmVnaW4KICAgIGFkZENsYXNzKENvbXBpbGVyLCAnJENsYXNzTmFtZScsICckQ2xhc3NQYXJlbnQnKTsKCiAgICAkUmVnaXN0ZXJzCiAgZW5kOwplbmQ7CgplbmQuCg==').decode("utf-8")

class PointerDict(dict):		
	def __missing__(self, key):
		if (key[0] == 'T'):
			key = 'P' + key[1:]
		else:
			key = 'P' + key
		return key

Pointer = PointerDict({
	'string': 'PlpString',
	'String': 'PlpString'
})
	
def ParseParams(obj, x):
	if (x.strip() == ''):
		return ''
		
	i = 1
	ret = []
	
	x = x.split(';')
	for types in x:
		vars = types.split(':')
		if (len(vars) == 2):
			type = vars[1].split('=')[0].strip()
			vars = vars[0]
			if (type not in obj.Depends):
					obj.Depends.append(type)
			for var in vars.split(','):
				ret.append(Pointer[type] + '(Params^[' + str(i) + '])^')
				i += 1
	
	return ', '.join(ret)
		
def Method(obj, x, line):
	if not obj.inPublic:
		return
	if 'abstract;' in x:	
		return
	
	Overload = ''
	
	ProcName = x[1]
	
	if ProcName == 'Create':
		ProcName = 'Init'
	
	while (ProcName in obj.MNames):
		Overload = ' overload;'
		ProcName += 'Ex'
	obj.MNames.append(ProcName)
	
	if x[3] == None:
		x[3] = ''
	
	if (x[0].lower() == 'constructor'):
		obj.Registers.append("addGlobalFunc('procedure %s.Init(%s);%s', @%s_%s);" % (obj.Name, x[3], Overload, obj.Name, ProcName))		
		obj.Methods.append('''//%s
procedure %s_%s(const Params: PParamArray); lape_extdecl
begin
  %s(Params^[0])^ := %s.Create(%s);
end;''' % (line, obj.Name, ProcName, Pointer[obj.Name], obj.Name, ParseParams(obj, x[3])))
	
	if (x[0].lower() == 'procedure'):
		obj.Registers.append("addGlobalFunc('%s %s.%s(%s);%s', @%s_%s);" % (x[0].lower(), obj.Name, x[1], x[3], Overload, obj.Name, ProcName))
		obj.Methods.append('''//%s
procedure %s_%s(const Params: PParamArray); lape_extdecl
begin
  %s(Params^[0])^.%s(%s);
end;''' % (line, obj.Name, ProcName, Pointer[obj.Name], x[1], ParseParams(obj, x[3])))
		
	if (x[0].lower() == 'function'):
		obj.Registers.append("addGlobalFunc('%s %s.%s(%s): %s;%s', @%s_%s);" % (x[0].lower(), obj.Name, x[1], x[3], x[4], Overload, obj.Name, ProcName))
		obj.Methods.append('''//%s
procedure %s_%s(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  %s(Result)^ := %s(Params^[0])^.%s(%s);
end;''' % (line, obj.Name, ProcName, Pointer[x[4]], Pointer[obj.Name], x[1], ParseParams(obj, x[3])))

def Variable(obj, x, line):
	if not obj.inPublic:
		return
	
	if len(x) == 2:
		x.append(' ')
		x.append(' ')
		
	obj.Registers.append("addClassVar(Compiler, '%s', '%s', '%s', %s, %s);" % (obj.Name, x[0], x[1], '@%s_%s_Read' % (obj.Name, x[0]) if x[2] else 'nil', '@%s_%s_Write' % (obj.Name, x[0]) if x[3] else 'nil'))
	if (x[1] not in obj.Depends):
		obj.Depends.append(x[1])
	
	if x[2]:
		obj.Methods.append('''//Read: %s
procedure %s_%s_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  %s(Result)^ := %s(Params^[0])^.%s;
end;''' % (line, obj.Name, x[0], Pointer[x[1]], Pointer[obj.Name], x[0]))
	if x[3]:
		obj.Methods.append('''//Write: %s
procedure %s_%s_Write(const Params: PParamArray); lape_extdecl
begin
  %s(Params^[0])^.%s := %s(Params^[1])^;
end;''' % (line, obj.Name, x[0], Pointer[obj.Name], x[0], Pointer[x[1]]))
	
def Name(obj, x, line):
	obj.Name = x[0]
	obj.Depends.append(obj.Name)
	if x[1] and (not x[1] == ''):
		obj.Parent = x[1]
		obj.Depends.append(obj.Parent)

def Private(obj, x, line):
	obj.inPublic = False
	
def Public(obj, x, line):
	obj.inPublic = True
	
def End(obj, e, line):
	obj.Done = True

ClassDef = [Name, re.compile('^(.+?)\s*=\s*class\s*\(?(.*?)\)?$', re.I)]
PrivateDef = [Private, re.compile('^(private)$', re.I)]
PublicDef = [Public, re.compile('^(public)$', re.I)]
PublishedDef = [Public, re.compile('^(published)$', re.I)]
ProtectedDef = [Private, re.compile('^(protected)$', re.I)]
ProcedureDef = [Method, re.compile('^(procedure)\s+(.+?)\s*(\((.*?)\))*\s*;\s*(overload;)*?\s*(virtual;)*?\s*(abstract;)*?$', re.I)]
FunctionDef = [Method, re.compile('^(function)\s+(.+?)\s*(\((.*?)\))*?\s*:\s*(.+?)\s*;\s*(overload;)*?\s*(virtual;)*?\s*(abstract;)*?$', re.I)]
ConstructorDef = [Method, re.compile('^(constructor)\s+(.+?)\s*(\((.*?)\))*\s*;\s*(overload;)*?\s*(virtual;)*?\s*(abstract;)*?$', re.I)]
PropertyDef = [Variable, re.compile('^property\s+(.+?)\s*:\s*(.+?)\s+(read\s*.+?)*\s*(write\s*.+?)*\s*;(\s*default;)*?$', re.I)] 
VariableDef = [Variable, re.compile('^(.+?)\s*:\s*(.+?);$', re.I)]
EndDef = [End, re.compile('^end;$', re.I)]

Definitions = [ClassDef, PrivateDef, PublicDef, PublishedDef, ProtectedDef, ConstructorDef, ProcedureDef, FunctionDef, PropertyDef, VariableDef, EndDef]

class ClassParser:
	def __init__(self, classHeader = ''):
		self.classHeader = classHeader
		self.Name = 'TUnnamedClass'
		self.Parent = 'TObject'
		self.inPublic = True
		self.Done = False
		self.MNames = []
		self.Methods = []
		self.Registers = []
		self.Depends = []
	
	def Parse(self):
		lines = self.classHeader.split("\n")
		
		for line in lines:
			if (line.strip() == ''):
				continue
				
			for Definition in Definitions:
				x = Definition[1].match(line.strip())
				if x:
					Definition[0](self, list(x.groups()), line.strip())
					break
			else:
				print('Unknown Line:', line, file=sys.stderr)
			
			if (self.Done == True):
				break
		
		oldPublic = self.inPublic
		self.inPublic = True
		if ('Init' not in self.MNames):
			CreateMethod = 'constructor Create();'
			CreateList = list(ConstructorDef[1].match(CreateMethod).groups())
			ConstructorDef[0](self, CreateList, CreateMethod)
		
		FreeMethod = 'procedure Free();'
		FreeList = list(ProcedureDef[1].match(FreeMethod).groups())
		ProcedureDef[0](self, FreeList, FreeMethod)
		self.inPublic = oldPublic
	
	def Print(self):
		Template = string.Template(template)
		print(Template.safe_substitute(
			ClassName = self.Name,
			ClassParent = self.Parent,
			ClassNameP = Pointer[self.Name],
			Methods = '\n\n'.join(self.Methods),
			Registers = '\n    '.join(self.Registers),
			Depends = ', '.join(self.Depends)
		))

if __name__ == '__main__':
	Parser = ClassParser('''''')
	
	Parser.Parse()
	Parser.Print()