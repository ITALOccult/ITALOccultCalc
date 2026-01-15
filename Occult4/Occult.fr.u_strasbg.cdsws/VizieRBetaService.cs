using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Threading;
using System.Web.Services;
using System.Web.Services.Protocols;
using System.Xml.Serialization;
using Occult.Properties;

namespace Occult.fr.u_strasbg.cdsws
{
	[GeneratedCode("System.Web.Services", "4.8.9037.0")]
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[WebServiceBinding(Name = "VizieRBetaSoapBinding", Namespace = "urn:VizieRBeta")]
	public class VizieRBetaService : SoapHttpClientProtocol
	{
		private SendOrPostCallback ADQLrequestOperationCompleted;

		private SendOrPostCallback cataloguesMetaDataOperationCompleted;

		private SendOrPostCallback cataloguesMetaData1OperationCompleted;

		private SendOrPostCallback cataloguesDataOperationCompleted;

		private SendOrPostCallback cataloguesData1OperationCompleted;

		private SendOrPostCallback metaAllOperationCompleted;

		private SendOrPostCallback getAvailabilityOperationCompleted;

		private SendOrPostCallback ConeCatalogsOperationCompleted;

		private SendOrPostCallback ConeResultsOperationCompleted;

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

		public event ADQLrequestCompletedEventHandler ADQLrequestCompleted;

		public event cataloguesMetaDataCompletedEventHandler cataloguesMetaDataCompleted;

		public event cataloguesMetaData1CompletedEventHandler cataloguesMetaData1Completed;

		public event cataloguesDataCompletedEventHandler cataloguesDataCompleted;

		public event cataloguesData1CompletedEventHandler cataloguesData1Completed;

		public event metaAllCompletedEventHandler metaAllCompleted;

		public event getAvailabilityCompletedEventHandler getAvailabilityCompleted;

		public event ConeCatalogsCompletedEventHandler ConeCatalogsCompleted;

		public event ConeResultsCompletedEventHandler ConeResultsCompleted;

		public VizieRBetaService()
		{
			Url = Settings.Default.OccultUtilities_fr_u_strasbg_cdsws_VizieRBetaService;
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

		[SoapRpcMethod("", RequestNamespace = "urn:VizieRBeta", ResponseNamespace = "urn:VizieRBeta")]
		[return: SoapElement("return")]
		public string ADQLrequest(string adql)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("ADQLrequest", new object[1] { adql })[0];
		}

		public void ADQLrequestAsync(string adql)
		{
			ADQLrequestAsync(adql, null);
		}

		public void ADQLrequestAsync(string adql, object userState)
		{
			if (ADQLrequestOperationCompleted == null)
			{
				ADQLrequestOperationCompleted = OnADQLrequestOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("ADQLrequest", new object[1] { adql }, ADQLrequestOperationCompleted, userState);
		}

		private void OnADQLrequestOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.ADQLrequestCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.ADQLrequestCompleted(this, new ADQLrequestCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[SoapRpcMethod("", RequestNamespace = "urn:VizieRBeta", ResponseNamespace = "urn:VizieRBeta")]
		[return: SoapElement("return")]
		public string cataloguesMetaData(string target, double radius, string unit, string text)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("cataloguesMetaData", new object[4] { target, radius, unit, text })[0];
		}

		public void cataloguesMetaDataAsync(string target, double radius, string unit, string text)
		{
			cataloguesMetaDataAsync(target, radius, unit, text, null);
		}

		public void cataloguesMetaDataAsync(string target, double radius, string unit, string text, object userState)
		{
			if (cataloguesMetaDataOperationCompleted == null)
			{
				cataloguesMetaDataOperationCompleted = OncataloguesMetaDataOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("cataloguesMetaData", new object[4] { target, radius, unit, text }, cataloguesMetaDataOperationCompleted, userState);
		}

		private void OncataloguesMetaDataOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.cataloguesMetaDataCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.cataloguesMetaDataCompleted(this, new cataloguesMetaDataCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[WebMethod(MessageName = "cataloguesMetaData1")]
		[SoapRpcMethod("", RequestNamespace = "urn:VizieRBeta", ResponseNamespace = "urn:VizieRBeta")]
		[return: SoapElement("return")]
		public string cataloguesMetaData(string target, double radius, string unit, string text, string wavelength)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("cataloguesMetaData1", new object[5] { target, radius, unit, text, wavelength })[0];
		}

		public void cataloguesMetaData1Async(string target, double radius, string unit, string text, string wavelength)
		{
			cataloguesMetaData1Async(target, radius, unit, text, wavelength, null);
		}

		public void cataloguesMetaData1Async(string target, double radius, string unit, string text, string wavelength, object userState)
		{
			if (cataloguesMetaData1OperationCompleted == null)
			{
				cataloguesMetaData1OperationCompleted = OncataloguesMetaData1OperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("cataloguesMetaData1", new object[5] { target, radius, unit, text, wavelength }, cataloguesMetaData1OperationCompleted, userState);
		}

		private void OncataloguesMetaData1OperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.cataloguesMetaData1Completed != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.cataloguesMetaData1Completed(this, new cataloguesMetaData1CompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[SoapRpcMethod("", RequestNamespace = "urn:VizieRBeta", ResponseNamespace = "urn:VizieRBeta")]
		[return: SoapElement("return")]
		public string cataloguesData(string target, double radius, string unit, string text)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("cataloguesData", new object[4] { target, radius, unit, text })[0];
		}

		public void cataloguesDataAsync(string target, double radius, string unit, string text)
		{
			cataloguesDataAsync(target, radius, unit, text, null);
		}

		public void cataloguesDataAsync(string target, double radius, string unit, string text, object userState)
		{
			if (cataloguesDataOperationCompleted == null)
			{
				cataloguesDataOperationCompleted = OncataloguesDataOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("cataloguesData", new object[4] { target, radius, unit, text }, cataloguesDataOperationCompleted, userState);
		}

		private void OncataloguesDataOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.cataloguesDataCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.cataloguesDataCompleted(this, new cataloguesDataCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[WebMethod(MessageName = "cataloguesData1")]
		[SoapRpcMethod("", RequestNamespace = "urn:VizieRBeta", ResponseNamespace = "urn:VizieRBeta")]
		[return: SoapElement("return")]
		public string cataloguesData(string target, double radius, string unit, string text, string wavelength)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("cataloguesData1", new object[5] { target, radius, unit, text, wavelength })[0];
		}

		public void cataloguesData1Async(string target, double radius, string unit, string text, string wavelength)
		{
			cataloguesData1Async(target, radius, unit, text, wavelength, null);
		}

		public void cataloguesData1Async(string target, double radius, string unit, string text, string wavelength, object userState)
		{
			if (cataloguesData1OperationCompleted == null)
			{
				cataloguesData1OperationCompleted = OncataloguesData1OperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("cataloguesData1", new object[5] { target, radius, unit, text, wavelength }, cataloguesData1OperationCompleted, userState);
		}

		private void OncataloguesData1OperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.cataloguesData1Completed != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.cataloguesData1Completed(this, new cataloguesData1CompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[SoapRpcMethod("", RequestNamespace = "urn:VizieRBeta", ResponseNamespace = "urn:VizieRBeta")]
		[return: SoapElement("return")]
		public string metaAll()
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("metaAll", new object[0])[0];
		}

		public void metaAllAsync()
		{
			metaAllAsync(null);
		}

		public void metaAllAsync(object userState)
		{
			if (metaAllOperationCompleted == null)
			{
				metaAllOperationCompleted = OnmetaAllOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("metaAll", new object[0], metaAllOperationCompleted, userState);
		}

		private void OnmetaAllOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.metaAllCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.metaAllCompleted(this, new metaAllCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[SoapRpcMethod("", RequestNamespace = "http://VizieRWS", ResponseNamespace = "urn:VizieRBeta")]
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

		[SoapRpcMethod("", RequestNamespace = "http://VizieRWS", ResponseNamespace = "urn:VizieRBeta")]
		[return: SoapElement("ConeCatalogsReturn")]
		public string ConeCatalogs(VizieRCriteria in0, VizieRTarget in1, string in2, VizieRFilter in3)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("ConeCatalogs", new object[4] { in0, in1, in2, in3 })[0];
		}

		public void ConeCatalogsAsync(VizieRCriteria in0, VizieRTarget in1, string in2, VizieRFilter in3)
		{
			ConeCatalogsAsync(in0, in1, in2, in3, null);
		}

		public void ConeCatalogsAsync(VizieRCriteria in0, VizieRTarget in1, string in2, VizieRFilter in3, object userState)
		{
			if (ConeCatalogsOperationCompleted == null)
			{
				ConeCatalogsOperationCompleted = OnConeCatalogsOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("ConeCatalogs", new object[4] { in0, in1, in2, in3 }, ConeCatalogsOperationCompleted, userState);
		}

		private void OnConeCatalogsOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.ConeCatalogsCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.ConeCatalogsCompleted(this, new ConeCatalogsCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
			}
		}

		[SoapRpcMethod("", RequestNamespace = "http://VizieRWS", ResponseNamespace = "urn:VizieRBeta")]
		[return: SoapElement("ConeResultsReturn")]
		public string ConeResults(VizieRCriteria in0, VizieRTarget in1, string in2, VizieRFilter in3)
		{
			return (string)((SoapHttpClientProtocol)this).Invoke("ConeResults", new object[4] { in0, in1, in2, in3 })[0];
		}

		public void ConeResultsAsync(VizieRCriteria in0, VizieRTarget in1, string in2, VizieRFilter in3)
		{
			ConeResultsAsync(in0, in1, in2, in3, null);
		}

		public void ConeResultsAsync(VizieRCriteria in0, VizieRTarget in1, string in2, VizieRFilter in3, object userState)
		{
			if (ConeResultsOperationCompleted == null)
			{
				ConeResultsOperationCompleted = OnConeResultsOperationCompleted;
			}
			((SoapHttpClientProtocol)this).InvokeAsync("ConeResults", new object[4] { in0, in1, in2, in3 }, ConeResultsOperationCompleted, userState);
		}

		private void OnConeResultsOperationCompleted(object arg)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			if (this.ConeResultsCompleted != null)
			{
				InvokeCompletedEventArgs val = (InvokeCompletedEventArgs)arg;
				this.ConeResultsCompleted(this, new ConeResultsCompletedEventArgs(val.get_Results(), ((AsyncCompletedEventArgs)(object)val).Error, ((AsyncCompletedEventArgs)(object)val).Cancelled, ((AsyncCompletedEventArgs)(object)val).UserState));
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
