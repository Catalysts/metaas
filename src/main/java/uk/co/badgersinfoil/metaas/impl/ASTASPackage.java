/*
 * ASTASPackage.java
 * 
 * Copyright (c) 2006-2008 David Holroyd
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

import java.util.ArrayList;
import java.util.List;
import org.asdt.core.internal.antlr.AS3Parser;
import uk.co.badgersinfoil.metaas.dom.ASPackage;
import uk.co.badgersinfoil.metaas.dom.ASType;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;


public class ASTASPackage extends ASTScriptElement implements ASPackage {
	
	public ASTASPackage(LinkedListTree ast) {
		super(ast);
	}

	public String getName() {
		LinkedListTree first = ast.getFirstChild();
		if (first.getType() == AS3Parser.IDENTIFIER) {
			return ASTUtils.identText(first);
		}
		return null;
	}

	public void setName(String name) {
		ASTIterator i = new ASTIterator(ast);
		LinkedListTree first = i.next();
		// null name given, so remove any existing name,
		if (name == null && first.getType() == AS3Parser.IDENTIFIER) {
			i.remove();
			return;
		}
		// otherwise, add or replace the name given,
		LinkedListTree newName = AS3FragmentParser.parseIdent(name);
		if (first.getType() == AS3Parser.IDENTIFIER) {
			i.replace(newName);
		} else {
			i.insertBeforeCurrent(newName);
			newName.appendToken(TokenBuilder.newSpace());
		}
	}

	public void addImport(String name) {
		LinkedListTree imp = AS3FragmentParser.parseImport(name);
		int pos = findNextImportInsertionPoint();
		ASTUtils.addChildWithIndentation(getPkgBlockNode(), pos, imp);
	}

	private int findNextImportInsertionPoint() {
		ASTIterator i = getPkgBlockIter();
		int index = 0;
		while (i.search(AS3Parser.IMPORT) != null) {
			index = i.getCurrentIndex() + 1;
		}
		return index;
	}

	public List findImports() {
		ASTIterator i = getPkgBlockIter();
		LinkedListTree imp;
		List result = new ArrayList();
		while ((imp = i.search(AS3Parser.IMPORT)) != null) {
			result.add(importText(imp));
		}
		return result;
	}

	private String importText(LinkedListTree imp) {
		return ASTUtils.identStarText(imp.getFirstChild());
	}

	public boolean removeImport(String name) {
		ASTIterator i = getPkgBlockIter();
		LinkedListTree imp;
		while ((imp = i.search(AS3Parser.IMPORT)) != null) {
			if (importText(imp).equals(name)) {
				i.remove();
				return true;
			}
		}
		return false;
	}

	public ASType getType() {
		LinkedListTree block = ASTUtils.findChildByType(ast, AS3Parser.BLOCK);
		LinkedListTree type = ASTUtils.findChildByType(block, AS3Parser.CLASS_DEF);
		if (type != null) {
			return new ASTASClassType(type);
		}
		type = ASTUtils.findChildByType(block, AS3Parser.INTERFACE_DEF);
		if (type != null) {
			return new ASTASInterfaceType(type);
		}
		return null;
	}

	private LinkedListTree getPkgBlockNode() {
		return ASTUtils.findChildByType(ast, AS3Parser.BLOCK);
	}

	private ASTIterator getPkgBlockIter() {
		return new ASTIterator(getPkgBlockNode());
	}
}
