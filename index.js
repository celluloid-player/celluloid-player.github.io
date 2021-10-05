document.addEventListener('DOMContentLoaded', (event) => {

    //function to auto-update current year
    let current_year = () => {
        return new Date().getFullYear();
    }
    document.getElementById("current-year").innerHTML = current_year();
});
