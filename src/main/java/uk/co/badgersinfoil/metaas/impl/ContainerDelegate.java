/*
 * ContainerDelegate.java
 * 
 * Copyright (c) 2008 David Holroyd
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

import java.util.List;
import uk.co.badgersinfoil.metaas.dom.ASBreakStatement;
import uk.co.badgersinfoil.metaas.dom.ASContinueStatement;
import uk.co.badgersinfoil.metaas.dom.ASDeclarationStatement;
import uk.co.badgersinfoil.metaas.dom.ASDefaultXMLNamespaceStatement;
import uk.co.badgersinfoil.metaas.dom.ASDoWhileStatement;
import uk.co.badgersinfoil.metaas.dom.Expression;
import uk.co.badgersinfoil.metaas.dom.ASExpressionStatement;
import uk.co.badgersinfoil.metaas.dom.ASForEachInStatement;
import uk.co.badgersinfoil.metaas.dom.ASForInStatement;
import uk.co.badgersinfoil.metaas.dom.ASForStatement;
import uk.co.badgersinfoil.metaas.dom.ASIfStatement;
import uk.co.badgersinfoil.metaas.dom.ASReturnStatement;
import uk.co.badgersinfoil.metaas.dom.ASSuperStatement;
import uk.co.badgersinfoil.metaas.dom.ASSwitchStatement;
import uk.co.badgersinfoil.metaas.dom.ASThrowStatement;
import uk.co.badgersinfoil.metaas.dom.ASTryStatement;
import uk.co.badgersinfoil.metaas.dom.ASWhileStatement;
import uk.co.badgersinfoil.metaas.dom.ASWithStatement;
import uk.co.badgersinfoil.metaas.dom.Statement;
import uk.co.badgersinfoil.metaas.dom.StatementContainer;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;

public abstract class ContainerDelegate extends ASTScriptElement implements StatementContainer {

	public ContainerDelegate(LinkedListTree ast) {
		super(ast);
	}

	protected abstract StatementContainer getStatementContainer();

	public Statement addStmt(String statement) {
		return getStatementContainer().addStmt(statement);
	}

	public ASExpressionStatement newExprStmt(String expr) {
		return getStatementContainer().newExprStmt(expr);
	}
	public ASExpressionStatement newExprStmt(Expression expr) {
		return getStatementContainer().newExprStmt(expr);
	}

	public void addComment(String text) {
		getStatementContainer().addComment(text);
	}

	public ASIfStatement newIf(String condition) {
		return getStatementContainer().newIf(condition);
	}
	public ASIfStatement newIf(Expression condition) {
		return getStatementContainer().newIf(condition);
	}

	public ASForStatement newFor(String init, String condition, String update) {
		return getStatementContainer().newFor(init, condition, update);
	}
	public ASForStatement newFor(Expression init, Expression condition, Expression update) {
		return getStatementContainer().newFor(init, condition, update);
	}

	public ASForInStatement newForIn(String init, String list) {
		return getStatementContainer().newForIn(init, list);
	}
	public ASForInStatement newForIn(Expression init, Expression list) {
		return getStatementContainer().newForIn(init, list);
	}

	public ASForEachInStatement newForEachIn(String init, String list) {
		return getStatementContainer().newForEachIn(init, list);
	}
	public ASForEachInStatement newForEachIn(Expression init, Expression list) {
		return getStatementContainer().newForEachIn(init, list);
	}


	public ASWhileStatement newWhile(String condition) {
		return getStatementContainer().newWhile(condition);
	}
	public ASWhileStatement newWhile(Expression condition) {
		return getStatementContainer().newWhile(condition);
	}

	public ASDoWhileStatement newDoWhile(String condition) {
		return getStatementContainer().newDoWhile(condition);
	}
	public ASDoWhileStatement newDoWhile(Expression condition) {
		return getStatementContainer().newDoWhile(condition);
	}

	public ASSwitchStatement newSwitch(String condition) {
		return getStatementContainer().newSwitch(condition);
	}
	public ASSwitchStatement newSwitch(Expression condition) {
		return getStatementContainer().newSwitch(condition);
	}

	public ASWithStatement newWith(String expr) {
		return getStatementContainer().newWith(expr);
	}
	public ASWithStatement newWith(Expression expr) {
		return getStatementContainer().newWith(expr);
	}

	public ASDeclarationStatement newDeclaration(String assignment) {
		return getStatementContainer().newDeclaration(assignment);
	}
	public ASDeclarationStatement newDeclaration(Expression assignment) {
		return getStatementContainer().newDeclaration(assignment);
	}

	public ASReturnStatement newReturn(String expr) {
		return getStatementContainer().newReturn(expr);
	}
	public ASReturnStatement newReturn(Expression expr) {
		return getStatementContainer().newReturn(expr);
	}
	public ASReturnStatement newReturn() {
		return getStatementContainer().newReturn();
	}

	public ASSuperStatement newSuper(List args) {
		return getStatementContainer().newSuper(args);
	}

	public ASBreakStatement newBreak() {
		return getStatementContainer().newBreak();
	}

	public ASTryStatement newTryCatch(String var, String type) {
		return getStatementContainer().newTryCatch(var, type);
	}

	public ASTryStatement newTryFinally() {
		return getStatementContainer().newTryFinally();
	}

	public ASContinueStatement newContinue() {
		return getStatementContainer().newContinue();
	}

	public ASThrowStatement newThrow(Expression t) {
		return getStatementContainer().newThrow(t);
	}
	
	public ASDefaultXMLNamespaceStatement newDefaultXMLNamespace(String namespace) {
		return getStatementContainer().newDefaultXMLNamespace(namespace);
	}

	public boolean containsCode() {
		return getStatementContainer().containsCode();
	}
	
	public List getStatementList() {
		return getStatementContainer().getStatementList();
	}
}