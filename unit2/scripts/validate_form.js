  function validateForm() {
    var valid = true;
    if (['January','February','March','April','May','June','July','August','September','October','November','December']
      .indexOf(document.forms.birthday.elements.month.value) === -1) {
      valid = false;
      document.getElementById('error_month').textContent = document.forms.birthday.elements.month.value + ' is not a valid month';
    } else {
      document.getElementById('error_month').textContent = ''
    }
    if (isNaN(document.forms.birthday.elements.day.value) ||
        document.forms.birthday.elements.day.value < 1 ||
        document.forms.birthday.elements.day.value > 31) {
      valid = false;
      document.getElementById('error_day').textContent = document.forms.birthday.elements.day.value + ' is not a valid day of the month';
    } else {
      document.getElementById('error_day').textContent = '';
    }
    if (isNaN(document.forms.birthday.elements.year.value) ||
        document.forms.birthday.elements.year.value < 1890 || 
        document.forms.birthday.elements.year.value > 2030) {
      valid = false;
      document.getElementById('error_year').textContent = document.forms.birthday.elements.year.value + ' is not a likely birth year';
    } else {
      document.getElementById('error_year').textContent = '';
    }
    return valid;
  }

