const addResponseDisplay = target => {
  const container = document.createElement('div');
  const title = document.createElement('h2');
  title.innerText = "Response:";
  const writeTo = document.createElement('div');
  container.appendChild(title);
  container.appendChild(writeTo);
  target.parentNode.appendChild(container);

  return {
    display: content => writeTo.innerHTML = content
  }
}

const fetchAndDisplay = (form, onSubmit) => {
  const { display } = addResponseDisplay(form);

  form.addEventListener('submit', async event => {
    event.preventDefault();
    onSubmit().then(response => response.text())
      .then(body => {
        try {
          return JSON.parse(body);
        } catch {
          throw body;
        }
      }).then(display).catch(display);
  });
}

const getForm = document.getElementById('get-user');
fetchAndDisplay(getForm, () => {
  const { elements: {
    id: { value: id } }
  } = getForm;

  return fetch(`/user/${id}`);
});

const postForm = document.getElementById('post-user');
fetchAndDisplay(
  postForm,
  () => {
    const { elements: {
      username: { value: username },
      password: { value: password }
    } } = postForm;

    return fetch('/user', {
      method: 'POST',
      body: JSON.stringify({ username, password })
    });
  }
);
