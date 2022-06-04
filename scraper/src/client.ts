import axios from 'axios';

const client = axios.create({
    baseURL: 'https://krazydad.com/play/akari/',
    responseType: 'text',
    
    transitional: {
        forcedJSONParsing: false,
    },
});

export default client;