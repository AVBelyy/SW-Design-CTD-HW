package ru.akirakozov.sd.refactoring.servlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.Statement;

/**
 * @author akirakozov
 * @author anton
 */
public class GetProductsServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
        throws IOException {
        ServletCommon.withDbConn(
            response,
            (Statement stmt) -> {
                ResultSet rs = stmt.executeQuery("SELECT * FROM PRODUCT");
                ServletCommon.outputItemList(response, rs, "All items that we have");
            });
    }
}
