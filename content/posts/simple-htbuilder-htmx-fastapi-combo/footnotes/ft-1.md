React JSX (JavaScript)
```jsx
const jsx = (
  <div id="container">
    <ul className="greetings">
      {["hello", "hi", "whattup"].map((greeting) => (
        <li key={greeting}>{greeting}</li>
      ))}
    </ul>
  </div>
);
```

Jinja (Python)
```html
<div id="container">
  <ul class="greetings">
    {% for greeting in ["hello", "hi", "whattup"] %}
      <li>{{ greeting }}</li>
    {% endfor %}
  </ul>
</div>
```
Hugo Templating (Go)

```html
<div id="container">
  <ul class="greetings">
    {{ range slice "hello" "hi" "whattup" }}
      <li>{{ . }}</li>
    {{ end }}
  </ul>
</div>
```