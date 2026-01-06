import http.server
import mimetypes
import socketserver
from pathlib import Path

mimetypes.add_type("application/wasm", ".wasm")

root = Path(__file__).resolve().parent

PORT = 8000

with socketserver.TCPServer(
    ("", PORT),
    lambda *a, **kw: http.server.SimpleHTTPRequestHandler(
        *a, directory=str(root), **kw
    ),
) as httpd:
    httpd.allow_reuse_address = True
    print(f"Serving {root} on http://localhost:{PORT}")
    httpd.serve_forever()
