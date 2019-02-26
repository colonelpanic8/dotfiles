import asyncio
import random
import queue
import requests
import threading

class WebhookHandler(object):

    def __init__(self, callback_uri='http://whatever'):
        self.queue = queue.Queue()
        self.callback_uri = callback_uri

    def enqueue(self, webhook_request):
        self.queue.put(webhook_request)

    def webhook_worker(self):
        while True:
            callback_request = self.queue.get()
            self.run_request(callback_request)
            self.queue.task_done()

    def flaky_request(self, callback_request):
        random_value = random.random()
        print(random_value)
        if random_value > .9:
            return 500
        r = requests.get(self.callback_uri, params=callback_request)
        return r.status_code

    def run_request(self, request):
        status_code = self.flaky_request(request)
        if status_code != 200:
            asyncio.run(self.do_retry(callback_request))
        else:
            print("made request")

    async def do_retry(self, request, delay=1):
        await asyncio.sleep(delay)
        print("Retried request")
        self.run_request(request)


if __name__ == '__main__':
    handler = WebhookHandler(callback_uri="https://www.google.com/")
    thread_count = 10
    for _ in range(1000):
        handler.enqueue({})

    threads = []
    for _ in range(thread_count):
        thread = threading.Thread(target=handler.webhook_worker, daemon=False)
        thread.start()
        threads.append(thread)

    for _ in range(1000):
        handler.enqueue({})

    loop = asyncio.get_event_loop()
    try:
        loop.run_forever()
    finally:
        loop.close()
