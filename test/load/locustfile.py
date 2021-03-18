from locust import HttpUser, TaskSet, task, between

from generator.subjects import get_random_subject, get_invalid_subject
from generator.properties import get_random_property, get_invalid_property

class QueryUser(HttpUser):
    host = "https://metadata.cardano-testnet.iohkdev.io"
    wait_time = between(0, 3)

    def on_start(self):
        self.wait()
        self.get_metadata_subject()
        self.get_metadata_subject_invalid()
        self.get_metadata_subject_properties()
        self.get_metadata_subject_properties_invalid()
        self.get_metadata_subject_properties_property()
        self.get_metadata_subject_properties_property_invalid()
        self.get_metadata_query()

    @task(1)
    def get_metadata_subject(self):
        subject = get_random_subject()
        
        print(f"GET /metadata/{subject}")

        with self.client.get(f"/metadata/{subject}", catch_response=True) as response:
            if response.elapsed.total_seconds() > 1:
                response.failure("Request took too long")

    @task(1)
    def get_metadata_subject_invalid(self):
        subject = get_invalid_subject()

        print(f"GET /metadata/{subject}")

        with self.client.get(f"/metadata/{subject}", catch_response=True) as response:
            if response.elapsed.total_seconds() > 1:
                response.failure("Request took too long")
            elif response.status_code == 404:
                response.success()

    @task(1)
    def get_metadata_subject_properties(self):
        subject = get_random_subject()

        print(f"GET /metadata/{subject}/properties")

        with self.client.get(f"/metadata/{subject}/properties", catch_response=True) as response:
            if response.elapsed.total_seconds() > 1:
                response.failure("Request took too long")

    @task(1)
    def get_metadata_subject_properties_invalid(self):
        subject = get_invalid_subject()

        print(f"GET /metadata/{subject}/properties")

        with self.client.get(f"/metadata/{subject}/properties", catch_response=True) as response:
            if response.elapsed.total_seconds() > 1:
                response.failure("Request took too long")
            elif response.status_code == 404:
                response.success()


    @task(1)
    def get_metadata_subject_properties_property(self):
        subject = get_random_subject()
        property = get_random_property()

        print(f"GET /metadata/{subject}/properties/{property}")

        with self.client.get(f"/metadata/{subject}/properties/{property}", catch_response=True) as response:
            if response.elapsed.total_seconds() > 1:
                response.failure("Request took too long")

    @task(1)
    def get_metadata_subject_properties_property_invalid(self):
        subject = get_invalid_subject()
        property = get_invalid_property()

        print(f"GET /metadata/{subject}/properties/{property}")

        with self.client.get(f"/metadata/{subject}/properties/{property}", catch_response=True) as response:
            if response.elapsed.total_seconds() > 1:
                response.failure("Request took too long")
            elif response.status_code == 404:
                response.success()

    @task(1)
    def get_metadata_query(self):
        print("GET /metadata/query")
        response = self.client.get(f"/metadata/query")

        with self.client.get(f"/metadata/query", catch_response=True) as response:
            if response.elapsed.total_seconds() > 1:
                response.failure("Request took too long")
