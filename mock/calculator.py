import sys
from datetime import datetime

import tornado.ioloop
import tornado.web
from tornado.log import enable_pretty_logging

enable_pretty_logging()


class APIHandler(tornado.web.RequestHandler):

    def set_default_headers(self):
        self.set_header("Content-Type", 'application/json')

    def get(self):
        print("request received", datetime.now())
        print(f"{self.request} {self.request.body.decode()}")
        self.set_status(200)
        self.write("OK")

    def post(self):
        print("request received", datetime.now())
        print(f"{self.request} {self.request.body.decode()}")
        self.set_status(200)
        self.write("OK")


def make_app():
    return tornado.web.Application([
        (r"/configure", APIHandler),
    ])


if __name__ == "__main__":
    default_port = '9095' if len(sys.argv) == 1 else sys.argv[1]
    port = int(default_port)
    app = make_app()
    app.listen(port)
    print(f"afm-calculator running on port {default_port} ...")
    tornado.ioloop.IOLoop.current().start()
