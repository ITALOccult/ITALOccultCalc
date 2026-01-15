using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Threading;
using System.Web.Services;
using System.Web.Services.Protocols;
using System.Xml.Serialization;
using Occult.Properties;

namespace Occult.SesameService
{
	[GeneratedCode("System.Web.Services", "4.8.9037.0")]
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[WebServiceBinding(Name = "SesameSoapBinding", Namespace = "urn:Sesame")]
	public class SesameService : SoapHttpClientProtocol
	{
		private SendOrPostCallback sesameOperationCompleted;

		private SendOrPostCallback sesame1OperationCompleted;

		private SendOrPostCallback sesame2OperationCompleted;

		private SendOrPostCallback SesameXMLOperationCompleted;

		private SendOrPostCallback SesameOperationCompleted;

		private SendOrPostCallback mainOperationCompleted;

		private SendOrPostCallback getAvailabilityOperationCompleted;

		private bool useDefaultCredentialsSetExplicitly;

		public string Url
		{
			get
			{
				return ((WebClientProtocol)this).get_Url();
			}
			set
			{
				if (IsLocalFileSystemWebService(((WebClientProtocol)this).get_Url()) && !useDefaultCredentialsSetExplicitly && !IsLocalFileSystemWebService(value))
				{
					((WebClientProtocol)this).set_UseDefaultCredentials(false);
				}
				((WebClientProtocol)this).set_Url(value);
			}
		}

		public bool UseDefaultCredentials
		{
			get
			{
				return ((WebClientProtocol)this).get_UseDefaultCredentials();
			}
			set
			{
				((WebClientProtocol)this).set_UseDefaultCredentials(value);
				useDefaultCredentialsSetExplicitly = true;
			}
		}

		public event sesameCompletedEventHandler sesameCompleted;

		public event sesame1CompletedEventHandler sesame1Completed;

		public event sesame2CompletedEventHandler sesame2Completed;

		public event SesameXMLCompletedEventHandler SesameXMLCompleted;

		public event SesameCompletedEventHandler SesameCompleted;

		public event mainCompletedEventHandler mainCompleted;

		public event getAvailabilityCompletedEventHandler getAvailabilityCompleted;

		public SesameService()
		{
			Url = Settings.Default.OccultUtilities_fr_u_strasbg_cdsws_SesameService;
			if (IsLocalFileSystemWebService(Url))
			{
				UseDefaultCredentials = true;
				useDefaultCredentialsSetExplicitly = false;
			}
			else
			{
				useDefaultCredentialsSetExplicitly = true;
			}
		}

		[SoapRpcMethod("", RequestNamespace = "urn:Sesame", ResponseNamespace = "urn:Sesame")]
		[return: SoapElement("return")]
		public string sesame(string name, string resultType)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("sesame", new object[2] { name, resultType })[0];
		}

		public void sesameAsync(string name, string resultType)
		{
			sesameAsync(name, resultType, null);
		}

		public void sesameAsync(string name, string resultType, object userState)
		{
			if (sesameOperationCompleted == null)
			{
				sesameOperationCompleted = OnsesameOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("sesame", new object[2] { name, resultType }, sesameOperationCompleted, userState);
		}

		private void OnsesameOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.sesameCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.sesameCompleted(this, new sesameCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[WebMethod(MessageName = "sesame1")]
		[SoapRpcMethod("", RequestNamespace = "urn:Sesame", ResponseNamespace = "urn:Sesame")]
		[return: SoapElement("return")]
		public string sesame(string name, string resultType, bool all)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("sesame1", new object[3] { name, resultType, all })[0];
		}

		public void sesame1Async(string name, string resultType, bool all)
		{
			sesame1Async(name, resultType, all, null);
		}

		public void sesame1Async(string name, string resultType, bool all, object userState)
		{
			if (sesame1OperationCompleted == null)
			{
				sesame1OperationCompleted = Onsesame1OperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("sesame1", new object[3] { name, resultType, all }, sesame1OperationCompleted, userState);
		}

		private void Onsesame1OperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.sesame1Completed != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.sesame1Completed(this, new sesame1CompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[WebMethod(MessageName = "sesame2")]
		[SoapRpcMethod("", RequestNamespace = "urn:Sesame", ResponseNamespace = "urn:Sesame")]
		[return: SoapElement("return")]
		public string sesame(string name, string resultType, bool all, string service)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("sesame2", new object[4] { name, resultType, all, service })[0];
		}

		public void sesame2Async(string name, string resultType, bool all, string service)
		{
			sesame2Async(name, resultType, all, service, null);
		}

		public void sesame2Async(string name, string resultType, bool all, string service, object userState)
		{
			if (sesame2OperationCompleted == null)
			{
				sesame2OperationCompleted = Onsesame2OperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("sesame2", new object[4] { name, resultType, all, service }, sesame2OperationCompleted, userState);
		}

		private void Onsesame2OperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.sesame2Completed != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.sesame2Completed(this, new sesame2CompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[SoapRpcMethod("", RequestNamespace = "urn:Sesame", ResponseNamespace = "urn:Sesame")]
		[return: SoapElement("return")]
		public string SesameXML(string name)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("SesameXML", new object[1] { name })[0];
		}

		public void SesameXMLAsync(string name)
		{
			SesameXMLAsync(name, null);
		}

		public void SesameXMLAsync(string name, object userState)
		{
			if (SesameXMLOperationCompleted == null)
			{
				SesameXMLOperationCompleted = OnSesameXMLOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("SesameXML", new object[1] { name }, SesameXMLOperationCompleted, userState);
		}

		private void OnSesameXMLOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.SesameXMLCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.SesameXMLCompleted(this, new SesameXMLCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[SoapRpcMethod("", RequestNamespace = "urn:Sesame", ResponseNamespace = "urn:Sesame")]
		[return: SoapElement("return")]
		public string Sesame(string name)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("Sesame", new object[1] { name })[0];
		}

		public void SesameAsync(string name)
		{
			SesameAsync(name, null);
		}

		public void SesameAsync(string name, object userState)
		{
			if (SesameOperationCompleted == null)
			{
				SesameOperationCompleted = OnSesameOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("Sesame", new object[1] { name }, SesameOperationCompleted, userState);
		}

		private void OnSesameOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.SesameCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.SesameCompleted(this, new SesameCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[SoapRpcMethod("", RequestNamespace = "http://DefaultNamespace", ResponseNamespace = "urn:Sesame")]
		public void main(string[] in0)
		{
			((SoapHttpClientProtocol)this).Invoke("main", new object[1] { in0 });
		}

		public void mainAsync(string[] in0)
		{
			mainAsync(in0, null);
		}

		public void mainAsync(string[] in0, object userState)
		{
			if (mainOperationCompleted == null)
			{
				mainOperationCompleted = OnmainOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("main", new object[1] { in0 }, mainOperationCompleted, userState);
		}

		private void OnmainOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.mainCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.mainCompleted(this, new AsyncCompletedEventArgs(((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[SoapRpcMethod("", RequestNamespace = "http://DefaultNamespace", ResponseNamespace = "urn:Sesame")]
		[return: SoapElement("getAvailabilityReturn")]
		public string getAvailability()
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("getAvailability", new object[0])[0];
		}

		public void getAvailabilityAsync()
		{
			getAvailabilityAsync(null);
		}

		public void getAvailabilityAsync(object userState)
		{
			if (getAvailabilityOperationCompleted == null)
			{
				getAvailabilityOperationCompleted = OngetAvailabilityOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("getAvailability", new object[0], getAvailabilityOperationCompleted, userState);
		}

		private void OngetAvailabilityOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.getAvailabilityCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.getAvailabilityCompleted(this, new getAvailabilityCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		public void CancelAsync(object userState)
		{
			((HttpWebClientProtocol)this).CancelAsync(userState);
		}

		private bool IsLocalFileSystemWebService(string url)
		{
			if (url == null || url == string.Empty)
			{
				return false;
			}
			Uri uri = new Uri(url);
			if (uri.Port >= 1024 && string.Compare(uri.Host, "localHost", StringComparison.OrdinalIgnoreCase) == 0)
			{
				return true;
			}
			return false;
		}
	}
}
