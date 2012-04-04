/*
 * ActionScriptWalker.java
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

package uk.co.badgersinfoil.metaas.visitor;

import java.util.Iterator;
import java.util.List;
import uk.co.badgersinfoil.metaas.dom.*;


public class ActionScriptWalker implements ActionScriptVisitor {

	private ScriptElementStrategy strategy;

	public ActionScriptWalker(FilterStrategy strategy) {
		this.strategy = strategy;
		strategy.setFiltered(new ScriptElementSwitch(this));
	}

	/**
	 * @param elements a list of {@link ScriptElement}.
	 */
	public void walk(List elements) {
		for (Iterator i=elements.iterator(); i.hasNext(); ) {
			walk((ScriptElement)i.next());
		}
	}

	public void walk(ScriptElement e) {
		strategy.handle(e);
	}

	/**
	 * walks each statement in the container
	 */
	public void walkStatementContainer(StatementContainer container) {
		walk(container.getStatementList());
	}

	public void visitArg(ASArg arg) {
	}

	public void visitArrayAccessExpression(ASArrayAccessExpression expr) {
		walk(expr.getTargetExpression());
		walk(expr.getSubscriptExpression());
	}

	public void visitArrayLiteral(ASArrayLiteral lit) {
		walk(lit.getEntries());
	}

	public void visitAssignmentExpression(ASAssignmentExpression expr) {
		walk(expr.getLeftSubexpression());
		walk(expr.getRightSubexpression());
	}

	public void visitBinaryExpression(ASBinaryExpression expr) {
		walk(expr.getLeftSubexpression());
		walk(expr.getRightSubexpression());
	}

	public void visitBlockStatement(ASBlock stmt) {
		walkStatementContainer(stmt);
	}

	public void visitBooleanLiteral(ASBooleanLiteral lit) {
	}

	public void visitBreakStatement(ASBreakStatement stmt) {
	}

	public void visitCatchClause(ASCatchClause catchClause) {
		walkStatementContainer(catchClause);
	}

	public void visitClassType(ASClassType type) {
		walk(type.getAllMetaTags());
		walk(type.getFields());
		walk(type.getMethods());
	}

	public void visitCompilationUnit(ASCompilationUnit unit) {
		walk(unit.getPackage());
	}

	public void visitConditionalExpression(ASConditionalExpression expr) {
		walk(expr.getConditionExpression());
		walk(expr.getThenExpression());
		walk(expr.getElseExpression());
	}

	public void visitContinueStatement(ASContinueStatement stmt) {
	}

	public void visitDeclarationStatement(ASDeclarationStatement stmt) {
		walk(stmt.getVars());
	}

	public void visitDefaultXMLNamespaceStatement(ASDefaultXMLNamespaceStatement stmt) {
	}

	public void visitDoWhileStatement(ASDoWhileStatement stmt) {
		walk(stmt.getCondition());
		walkStatementContainer(stmt);
	}

	public void visitDescendantExpression(ASDescendantExpression expr) {
		walk(expr.getTarget());
		walk(expr.getQuery());
	}

	public void visitExpressionAttribute(ASExpressionAttribute expr) {
		walk(expr.getExpression());
	}

	public void visitExpressionStatement(ASExpressionStatement stmt) {
		walk(stmt.getExpression());
	}

	public void visitField(ASField field) {
		walk(field.getAllMetaTags());
	}

	public void visitFieldAccessExpression(ASFieldAccessExpression expr) {
		walk(expr.getTargetExpression());
	}

	public void visitFilterExpression(ASFilterExpression expr) {
		walk(expr.getTarget());
		walk(expr.getQuery());
	}

	public void visitFinallyClause(ASFinallyClause fin) {
		walkStatementContainer(fin);
	}

	public void visitForEachInStatement(ASForEachInStatement stmt) {
		// TODO: stmt.getVar();
		walk(stmt.getIterated());
		walkStatementContainer(stmt);
	}

	public void visitForInStatement(ASForInStatement stmt) {
		// TODO: stmt.getVar();
		walk(stmt.getIterated());
		walkStatementContainer(stmt);
	}

	public void visitForStatement(ASForStatement stmt) {
		// TODO: stmt.getInit();
		Expression condition = stmt.getCondition();
		if (condition != null) {
			walk(condition);
		}
		Expression update = stmt.getUpdate();
		if (update != null) {
			walk(update);
		}
		walkStatementContainer(stmt);
	}

	public void visitFunctionExpression(ASFunctionExpression expr) {
		walk(expr.getArgs());
		walkStatementContainer((expr));
	}

	public void visitIfStatement(ASIfStatement stmt) {
		walk(stmt.getCondition());
		walk(stmt.getThenStatement());
		Statement elseStmt = stmt.getElseStatement();
		if (elseStmt != null) {
			walk(elseStmt);
		}
	}

	public void visitIntegerLiteral(ASIntegerLiteral lit) {
	}

	public void visitInterfaceType(ASInterfaceType type) {
		walk(type.getAllMetaTags());
		walk(type.getMethods());
	}

	public void visitInvocationExpression(ASInvocationExpression expr) {
		walk(expr.getTargetExpression());
		walk(expr.getArguments());
	}

	public void visitMetaTag(ASMetaTag tag) {
	}

	public void visitMethod(ASMethod method) {
		walk(method.getAllMetaTags());
		walk(method.getArgs());
		walkStatementContainer(method);
	}

	public void visitNewExpression(ASNewExpression expr) {
		walk(expr.getTargetExpression());
		walk(expr.getArguments());
	}

	public void visitNullLiteral(ASNullLiteral lit) {
	}

	public void visitObjectLiteral(ASObjectLiteral lit) {
		walk(lit.getFields());
	}

	public void visitObjectField(ASObjectLiteral.Field field) {
		walk(field.getValue());
	}

	public void visitPackage(ASPackage pkg) {
		walk(pkg.getType());
	}

	public void visitPostfixExpression(ASPostfixExpression expr) {
		walk(expr.getSubexpression());
	}

	public void visitPrefixExpression(ASPrefixExpression expr) {
		walk(expr.getSubexpression());
	}

	public void visitPropertyAttribute(ASPropertyAttribute expr) {
	}

	public void visitRegexpLiteral(ASRegexpLiteral e) {
	}

	public void visitReturnStatement(ASReturnStatement stmt) {
		Expression ret = stmt.getExpression();
		if (ret != null) {
			walk(ret);
		}
	}

	public void visitSimpleNameExpression(ASSimpleNameExpression e) {
	}

	public void visitStringLiteral(ASStringLiteral lit) {
	}

	public void visitStarAttribute(ASStarAttribute expr) {
	}

	public void visitSuperStatement(ASSuperStatement stmt) {
		walk(stmt.getArguments());
	}

	public void visitSwitchCase(ASSwitchCase lab) {
		walk(lab.getLabelValue());
		walkStatementContainer(lab);
	}

	public void visitSwitchDefault(ASSwitchDefault lab) {
		walkStatementContainer(lab);
	}

	public void visitSwitchStatement(ASSwitchStatement stmt) {
		walk(stmt.getCondition());
		walk(stmt.getLabels());
	}

	public void visitThrowStatement(ASThrowStatement stmt) {
		walk(stmt.getExpression());
	}

	public void visitTryStatement(ASTryStatement stmt) {
		walkStatementContainer(stmt);
		List catchClauses = stmt.getCatchClauses();
		if (!catchClauses.isEmpty()) {
			walk(catchClauses);
		}
		ASFinallyClause fin = stmt.getFinallyClause();
		if (fin != null) {
			walk(fin);
		}
	}

	public void visitUndefinedLiteral(ASUndefinedLiteral lit) {
	}

	public void visitVarDeclarationFragment(ASVarDeclarationFragment var) {
		walk(var.getInitializer());
	}

	public void visitWhileStatement(ASWhileStatement stmt) {
		walk(stmt.getCondition());
		walk(stmt.getBody());
	}

	public void visitWithStatement(ASWithStatement stmt) {
		walk(stmt.getScope());
		walk(stmt.getBody());
	}

	public void visitXMLLiteral(ASXMLLiteral lit) {
	}
}
