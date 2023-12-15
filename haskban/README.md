# haskban
TODO:

Client:
1. Left and right movement should limit to max size of the column. Currently goes out of bounds. - DONE
2. Ctrl+l and ctrl + r should move do what? Reset to (0, 0) doesn't makes sense because if there are no tasks in todo, then it goes out of bounds. Can we change the shortcut to Ctrl+arrows?
3. Making radio buttons work
4. Some way to show what field I am currently editing. - DONE
5. Get information from server. Send information to server.
6. What would happen when I am looking at my board and someone else adds a task? how do i update it? Anything to refresh?
7. Unit tests using quickcheck or any other frameworks.
8. Improve UI. Can add few more information on the screen. Add name of the project on the top.
9. Scrolling. If there are more tasks than the screen can show, then should we be able to scroll?. Currently, can't see any tasks that are out of the screen. Can pick this up last. Instead of scrolling, whenever we reach out of bounds, refresh the screen such that the current task is at the top of the screen.
10. Backspace should work.
11. Can support deleting tasks.


Server:
1. send task id as well. Requried when updating a task.
2. will get issue when creating tasks because there is no task id
3. Unit tests using quickcheck or any other frameworks.
4. Implement PUT request.
5. implement DELETE request. 
