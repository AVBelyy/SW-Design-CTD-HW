package ru.akirakozov.sd.refactoring.servlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Arrays;
import java.util.List;

/**
 * @author akirakozov
 * @author anton
 */
public class QueryServlet extends HttpServlet {
    private List<String> validCommands = Arrays.asList("max",
            "min",
            "sum",
            "count");

    @Override
    protected void doGet(HttpServletRequest request,
                         HttpServletResponse response) throws IOException {
        String command = request.getParameter("command");
        if (validCommands.contains(command)) {
            ServletCommon.withDbConn(response, (Statement stmt) -> {
                if ("max".equals(command)) {
                    ResultSet rs = stmt.executeQuery("SELECT * FROM PRODUCT ORDER BY PRICE DESC LIMIT 1");
                    ServletCommon.outputItemList(response, rs, "Items with max price");
                } else if ("min".equals(command)) {
                    ResultSet rs = stmt.executeQuery("SELECT * FROM PRODUCT ORDER BY PRICE LIMIT 1");
                    ServletCommon.outputItemList(response, rs, "Items with min price");
                } else if ("sum".equals(command)) {
                    ResultSet rs = stmt.executeQuery("SELECT SUM(price) FROM PRODUCT");
                    ServletCommon.outputHtmlPage(response, printWriter -> {
                        if (rs.next()) {
                            printWriter.println(rs.getInt(1));
                        }
                    }, "Summary price");
                } else if ("count".equals(command)) {
                    ResultSet rs = stmt.executeQuery("SELECT COUNT(*) FROM PRODUCT");
                    ServletCommon.outputHtmlPage(response, printWriter -> {
                        if (rs.next()) {
                            printWriter.println(rs.getInt(1));
                        }
                    }, "Number of products");
                }
                });
        } else {
            response.getWriter().println("Unknown command: " + command);
            response.setContentType("text/html");
            response.setStatus(HttpServletResponse.SC_OK);
        }
    }
}
