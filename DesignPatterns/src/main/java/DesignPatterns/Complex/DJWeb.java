package DesignPatterns.Complex;


import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

@WebServlet(name = "DJWeb", urlPatterns = {"djweb"}, loadOnStartup = 1)
public class DJWeb extends HttpServlet {
    DJModel djModel;
    @Override
    public void init() throws ServletException{
        djModel = new DJModel();
        System.out.println("here");
    }

    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException{
        RequestDispatcher dispatcher = request.getRequestDispatcher("/DJWeb.jsp");
        try {
            int val = Integer.parseInt(request.getParameter("value"));
            djModel.incrBPM(val);
        }catch (Exception e){}

        request.setAttribute("bpm", djModel.getBpm());
        dispatcher.forward(request, response);
    }
}
