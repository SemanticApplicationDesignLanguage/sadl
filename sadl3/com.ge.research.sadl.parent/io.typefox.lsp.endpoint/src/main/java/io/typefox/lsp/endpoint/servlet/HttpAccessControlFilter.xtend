package io.typefox.lsp.endpoint.servlet

import com.google.inject.Singleton
import java.io.IOException
import javax.servlet.Filter
import javax.servlet.FilterChain
import javax.servlet.FilterConfig
import javax.servlet.ServletException
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.http.HttpServletResponse

@Singleton
class HttpAccessControlFilter implements Filter {

    override init(FilterConfig config) throws ServletException {
    }

    override destroy() {
    }

    override doFilter(
        ServletRequest request,
        ServletResponse response,
        FilterChain chain
    ) throws IOException, ServletException {
        if (response instanceof HttpServletResponse) {
            response.addHeader("Access-Control-Allow-Origin", "*");
            response.addHeader("Access-Control-Allow-Methods", "GET, POST, DELETE, PUT");
            response.addHeader("Access-Control-Allow-Headers", "Content-Type");
        }
        chain.doFilter(request, response)
    }

}
