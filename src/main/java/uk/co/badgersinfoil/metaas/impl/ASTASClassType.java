/*
 * ASTASClassType.java
 * 
 * Copyright (c) 2006-2007 David Holroyd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.co.badgersinfoil.metaas.impl;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import org.asdt.core.internal.antlr.AS3Parser;
import uk.co.badgersinfoil.metaas.dom.ASClassType;
import uk.co.badgersinfoil.metaas.dom.ASField;
import uk.co.badgersinfoil.metaas.dom.ASMethod;
import uk.co.badgersinfoil.metaas.dom.Visibility;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListToken;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;

public class ASTASClassType extends ASTASType implements ASClassType {

	private static final int EXTENDS_INDEX = 3;

	public ASTASClassType(LinkedListTree clazz) {
		super(clazz);
	}

	public ASMethod newMethod(String name, Visibility visibility, String returnType) {
		ASTASMethod method = ASTBuilder.newClassMethod(name, visibility, returnType);
		addMethod(method);
		return method;
	}
	
	public void addMethod(ASTASMethod method) {
		ASTUtils.addChildWithIndentation(findTypeBlock(), method.getAST());
	}

	private LinkedListTree findModifiers() {
		return ASTUtils.findChildByType(ast, AS3Parser.MODIFIERS);
	}
	
	public String getSuperclass() {
		LinkedListTree ext = ASTUtils.findChildByType(ast, AS3Parser.EXTENDS);
		if (ext == null) return null;
		return ASTUtils.identText(ext.getFirstChild());
	}

	public boolean isDynamic() {
		return ModifierUtils.isDynamic(findModifiers());
	}
	
	public boolean isFinal() {
		return ModifierUtils.isFinal(findModifiers());
	}

	public void setSuperclass(String superclassName) {
		if (superclassName == null) {
			removeExtendsClause();
			return;
		}
		LinkedListTree ext = ASTUtils.findChildByType(ast, AS3Parser.EXTENDS);
		LinkedListTree superIdent = AS3FragmentParser.parseIdent(superclassName);
		if (ext == null) {
			ext = ASTUtils.newAST(AS3Parser.EXTENDS, "extends");
			ext.appendToken(TokenBuilder.newSpace());
			// hack a space in at the right point,
			LinkedListToken sp = TokenBuilder.newSpace();
			ext.getStartToken().beforeInsert(sp);
			ext.setStartToken(sp);
			ast.addChildWithTokens(EXTENDS_INDEX, ext);
			ext.addChildWithTokens(superIdent);
			ext.appendToken(TokenBuilder.newSpace());
		} else {
			ext.setChildWithTokens(0, superIdent);
		}
	}

	private void removeExtendsClause() {
		for (ASTIterator i=new ASTIterator(ast); i.hasNext(); ) {
			LinkedListTree node = i.next();
			if (node.getType() == AS3Parser.EXTENDS) {
				i.remove();
				break;
			}
		}
	}

	public List getImplementedInterfaces() {
		List results = new LinkedList();
		LinkedListTree impls = ASTUtils.findChildByType(ast, AS3Parser.IMPLEMENTS);
		if (impls != null) {
			for (ASTIterator i=new ASTIterator(impls); i.hasNext(); ) {
				results.add(ASTUtils.identText(i.next()));
			}
		}
		return Collections.unmodifiableList(results);
	}

	public void addImplementedInterface(String interfaceName) {
		LinkedListTree iface = AS3FragmentParser.parseIdent(interfaceName);
		LinkedListTree impls = ASTUtils.findChildByType(ast, AS3Parser.IMPLEMENTS);
		if (impls == null) {
			ASTIterator i = new ASTIterator(ast);
			i.find(AS3Parser.TYPE_BLOCK);
			impls = ASTUtils.newAST(AS3Parser.IMPLEMENTS, "implements");
			i.insertBeforeCurrent(impls);
			LinkedListToken sp = TokenBuilder.newSpace();
			impls.getStartToken().beforeInsert(sp);
		} else {
			impls.appendToken(TokenBuilder.newComma());
		}
		impls.appendToken(TokenBuilder.newSpace());
		impls.addChildWithTokens(iface);
	}

	public void removeImplementedInterface(String interfaceName) {
		LinkedListTree impls = ASTUtils.findChildByType(ast, AS3Parser.IMPLEMENTS);
		int count = 0;
		for (ASTIterator i=new ASTIterator(impls); i.hasNext(); ) {
			LinkedListTree iface = i.next();
			String name = ASTUtils.identText(iface);
			if (name.equals(interfaceName)) {
				if (i.hasNext()) {
					ASTUtils.removeTrailingWhitespaceAndComma(iface.getStopToken());
				} else if (count == 0) {
					// no interfaces left, so remove the
					// 'implements' clause completely,
					ast.deleteChild(ast.getIndexOfChild(impls));
					break;
				}
				i.remove();
				if (i.hasNext()) {
					count++;
				}
				break;
			}
			count++;
		}
	}

	public ASField newField(String name, Visibility visibility, String type) {
		ASTASField field = ASTBuilder.newField(name, visibility, type);
		addField(field);
		return field;
	}
	
	public void addField(ASTASField field) {
		ASTUtils.addChildWithIndentation(findTypeBlock(), field.getAST());
	}

	public ASField getField(String name) {
		for (ASTIterator i=blockIter(); i.hasNext(); ) {
			LinkedListTree member = i.next();
			if (member.getType() == AS3Parser.VAR_DEF) {
				ASField field = new ASTASField(member);
				if (field.getName().equals(name)) {
					return field;
				}
			}
		}
		return null;
	}

	public List getFields() {
		List results = new LinkedList();
		for (ASTIterator i=blockIter(); i.hasNext(); ) {
			LinkedListTree member = i.next();
			if (member.getType() == AS3Parser.VAR_DEF) {
				results.add(new ASTASField(member));
			}
		}
		return Collections.unmodifiableList(results);
	}

	public void removeField(String name) {
		for (ASTIterator i=blockIter(); i.hasNext(); ) {
			LinkedListTree member = i.next();
			if (member.getType() == AS3Parser.VAR_DEF) {
				ASField field = new ASTASField(member);
				if (field.getName().equals(name)) {
					i.remove();
					return;
				}
			}
		}
	}
	
	public void setDynamic(boolean value) {
		ModifierUtils.setDynamic(findModifiers(), value);
	}

	public void setFinal(boolean value) {
		ModifierUtils.setFinal(findModifiers(), value);
	}

}
