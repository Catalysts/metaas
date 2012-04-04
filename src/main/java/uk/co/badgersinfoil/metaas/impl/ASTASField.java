/*
 * ASTASField.java
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

import org.asdt.core.internal.antlr.AS3Parser;
import uk.co.badgersinfoil.metaas.SyntaxException;
import uk.co.badgersinfoil.metaas.dom.Expression;
import uk.co.badgersinfoil.metaas.dom.ASField;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListToken;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;

// TODO: need to handle multi-field declarations like,
//       class X { var foo:String, bar:Number; }

public class ASTASField extends ASTASMember implements ASField {

	private static int INDEX_DEF = 2;
	private static int INDEX_DECL = 3;

	public ASTASField(LinkedListTree ast) {
		super(ast);
	}

	public String getName() {
		return findDecl().getText();
	}

	public void setName(String name) {
		if (name.indexOf('.') != -1) {
			throw new SyntaxException("field name must not contain '.'");
		}
		findDecl().getToken().setText(name);
	}

	public void setInitializer(String expr) {
		if (expr == null) {
			removeInitializer();
		} else {
			setInitAST(AS3FragmentParser.parseExpr(expr));
		}
	}

	public void setInitializer(Expression expr) {
		if (expr == null) {
			removeInitializer();
		} else {
			setInitAST(ast(expr));
		}
	}

	private void setInitAST(LinkedListTree exp) {
		LinkedListTree decl = findDecl();
		LinkedListTree init = ASTUtils.findChildByType(decl, AS3Parser.ASSIGN);
		if (init == null) {
			init = ASTUtils.newAST(AS3Parser.ASSIGN, "=");
			decl.addChildWithTokens(init);
		} else {
			init.deleteChild(0);
		}
		init.addChildWithTokens(exp);
	}

	private void removeInitializer() {
		LinkedListTree decl = findDecl();
		ASTIterator i = new ASTIterator(decl);
		if (i.search(AS3Parser.ASSIGN) != null) {
			i.remove();
		}
	}

	public Expression getInitializer() {
		LinkedListTree decl = findDecl();
		LinkedListTree init = ASTUtils.findChildByType(decl, AS3Parser.ASSIGN);
		if (init == null) {
			return null;
		}
		return ExpressionBuilder.build(init.getFirstChild());
	}

	public boolean isConst() {
		return ast.getChild(INDEX_DEF).getType() == AS3Parser.CONST;
	}

	public void setConst(boolean isConst) {
		if (isConst() == isConst) {
			return;
		}
		LinkedListToken node;
		if (isConst) {
			node = TokenBuilder.newConst();
		} else {
			node = TokenBuilder.newVar();
		}
		ast.setChildWithTokens(INDEX_DEF, ASTUtils.newAST(node));
	}

	public String getType() {
		LinkedListTree decl = findDecl();
		LinkedListTree typeSpec = decl.getFirstChild();
		if (typeSpec == null) return null;
		return ASTUtils.typeSpecText(typeSpec);
	}

	private LinkedListTree findDecl() {
		return (LinkedListTree)ast.getChild(INDEX_DECL);
	}

	public void setType(String typeName) {
		LinkedListTree decl = findDecl();
		LinkedListTree typeSpec = decl.getFirstChild();
		if (typeSpec == null) {
			if (typeName != null) {
				decl.addChildWithTokens(AS3FragmentParser.parseTypeSpec(typeName));
			}
		} else {
			if (typeName == null) {
				decl.deleteChild(0);
			} else {
				decl.setChildWithTokens(0, AS3FragmentParser.parseTypeSpec(typeName));
			}
		}
	}
}