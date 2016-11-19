package ru.akirakozov.sd.refactoring.servlet;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.ResultSet;

/**
 * @author anton
 */
class ServletCommon {
    static void outputHtmlPage(HttpServletResponse response, DBHandler<PrintWriter> pageGen, String title)
        throws IOException, SQLException {
        PrintWriter writer = response.getWriter();
        writer.println("<html><body>");
        writer.println("<h1>" + title + ": </h1>");
        pageGen.apply(writer);
        writer.println("</body></html>");
    }

    static void outputItemList(HttpServletResponse response, ResultSet rs, String title)
        throws IOException, SQLException {
        outputHtmlPage(response, printWriter -> {
            while (rs.next()) {
                String name = rs.getString("name");
                int price  = rs.getInt("price");
                printWriter.println(name + "\t" + price + "</br>");
            }
        }, title);
    }

    static void withDbConn(HttpServletResponse response, DBHandler<Statement> handler) {
        try {
            try (Connection c = DriverManager.getConnection("jdbc:sqlite:test.db")) {
                Statement stmt = c.createStatement();
                handler.apply(stmt);
                stmt.close();
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        response.setContentType("text/html");
        response.setStatus(HttpServletResponse.SC_OK);
    }
}
