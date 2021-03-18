from locust import HttpUser, TaskSet, task, between

from generator.subjects import get_random_subject

class QueryUser(HttpUser):
    host = "https://metadata.cardano-testnet.iohkdev.io"
    wait_time = between(5, 15)

    def on_start(self):
        self.wait()
        self.get_metadata()

    @task(3)
    def get_metadata(self):
        subject = get_random_subject()
        print(f"GET | /metadata/ | {subject}")
        response = self.client.get(f"/metadata/{subject}")
        print(f"Response: {response.status_code}")
