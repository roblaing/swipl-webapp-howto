function hexString(buffer) {
  const byteArray = new Uint8Array(buffer);
  const hexCodes = [...byteArray].map(value => {
    const hexCode = value.toString(16);
    const paddedHexCode = hexCode.padStart(2, '0');
    return paddedHexCode;
  });
  return hexCodes.join('');
}

function digestMessage(message) {
  const encoder = new TextEncoder();
  const data = encoder.encode(message);
  return window.crypto.subtle.digest('SHA-256', data);
}

function validateForm() {
  var valid = true;
  if (document.forms.signup.elements.username.value.length === 0) {
    valid = false;
    document.getElementById('error_username').textContent = 'No Username';
  } else {
    document.getElementById('error_username').textContent = ''
  }
  if (document.forms.signup.elements.password.value.length < 4) {
    valid = false;
    document.getElementById('error_password').textContent = 'Password too short';
  } else {
    document.getElementById('error_password').textContent = '';
  }
  if (document.forms.signup.elements.verify.value !== document.forms.signup.elements.password.value) {
    valid = false;
    document.getElementById('error_verify').textContent = 'Password does not match';
  } else {
    document.getElementById('error_verify').textContent = '';
  }
  if (valid) {
    const text = document.forms.signup.elements.username.value +
                 document.forms.signup.elements.password.value;
    digestMessage(text).then(digestValue => {
      document.cookie = 'user=' + hexString(digestValue);
    });
    document.forms.signup.elements.password.value = '';
    document.forms.signup.elements.verify.value = '';
  }
  return valid;
}

