** Title of API call **
--- 
<descr>

* **URL**

  /deref/#Alias

* **Method:**

  `GET`
  
*  **URL Params**

   **Required:**
 
   `Alias ~ String | Text`

* **Data Params**

  None

* **Success Response:**

  * **Code:** 200 <br />
    **Content:** 
    ```
    <h1>DefaultDerefTemplate</h1> <p>resp</p>
    <p>eureka!
    </p>
    ```


* **Error Response:**

  * **Code:** 200 <br />
    **Content:** 
    ```
    <h1>DefaultDerefTemplate</h1>
    <p>eau</p>
    <p>abbr not found in Abbrev
    </p>
    ```

    
* **Sample Call:**

  ```bash
  curl http::/localhost:3000/deref/spec
    
  ```

